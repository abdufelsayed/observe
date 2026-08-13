module Make (IO : Io.S) = struct
  type error =
    | Invalid_path
    | Invalid_capacity of int
    | Io of IO.error
    | Zero_progress
    | Invalid_write_count of int
    | Unexpected of exn

  type record = { sequence : int64; path : string; bytes : string }
  type write = { path : string; bytes : string; length : int; through : int64 }
  type status = Running | Closing | Failed of error | Stopped
  type write_buffer = { mutable bytes : Bytes.t; mutable view : string }

  type t = {
    path : string;
    capacity : int;
    lock : IO.lock;
    worker_notifier : IO.notifier;
    progress_notifier : IO.notifier;
    queue : record Queue.t;
    formatter : Observe.Formatter.t;
    mutable accepted_sequence : int64;
    mutable completed_sequence : int64;
    mutable flush_barriers : int64 list;
    mutable flushed_sequence : int64;
    mutable status : status;
    mutable terminal_error : error option;
    mutable current_file : (string * IO.file) option;
    mutable worker_waiting : bool;
    write_buffer : write_buffer;
  }

  let ( let* ) = IO.bind

  let pp_error formatter = function
    | Invalid_path -> Format.pp_print_string formatter "invalid filesystem path"
    | Invalid_capacity capacity ->
        Format.fprintf formatter "invalid queue capacity %d" capacity
    | Io error -> IO.pp_error formatter error
    | Zero_progress ->
        Format.pp_print_string formatter "filesystem write made no progress"
    | Invalid_write_count count ->
        Format.fprintf formatter "filesystem returned invalid write count %d"
          count
    | Unexpected exn ->
        Format.fprintf formatter "unexpected filesystem exception: %s"
          (Printexc.to_string exn)

  let contains_nul value =
    let rec loop index =
      index < String.length value
      && (String.unsafe_get value index = '\000' || loop (index + 1))
    in
    loop 0

  let preserve_fatal = function
    | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> raise exn
    | _ -> ()

  let initial_buffer_size = min Sys.max_string_length (256 * 1_024)

  let create_write_buffer () =
    let bytes = Bytes.create initial_buffer_size in
    { bytes; view = Bytes.unsafe_to_string bytes }

  let ensure_buffer_capacity buffer ~used required =
    if required > Bytes.length buffer.bytes then (
      let rec grow capacity =
        if capacity >= required then capacity
        else if capacity > Sys.max_string_length / 2 then Sys.max_string_length
        else grow (capacity * 2)
      in
      let capacity = grow (max 1 (Bytes.length buffer.bytes)) in
      let bytes = Bytes.create capacity in
      Bytes.blit buffer.bytes 0 bytes 0 used;
      buffer.bytes <- bytes;
      buffer.view <- Bytes.unsafe_to_string bytes)

  let append buffer offset value =
    let length = String.length value in
    let required = offset + length in
    ensure_buffer_capacity buffer ~used:offset required;
    Bytes.blit_string value 0 buffer.bytes offset length;
    required

  let attempt callback =
    IO.catch
      (fun () ->
        let* outcome = callback () in
        IO.return (Result.map_error (fun error -> Io error) outcome))
      (fun exn ->
        preserve_fatal exn;
        IO.return (Error (Unexpected exn)))

  let close_current t =
    match t.current_file with
    | None -> IO.return (Ok ())
    | Some (_, file) ->
        let* outcome = attempt (fun () -> IO.close file) in
        (match outcome with Ok () -> t.current_file <- None | Error _ -> ());
        IO.return outcome

  let flush_current t =
    match t.current_file with
    | None -> IO.return (Ok ())
    | Some (_, file) -> attempt (fun () -> IO.flush file)

  let rec write_all file bytes offset limit =
    let length = limit - offset in
    if length = 0 then IO.return (Ok ())
    else
      let* outcome = attempt (fun () -> IO.write file bytes ~offset ~length) in
      match outcome with
      | Error error -> IO.return (Error error)
      | Ok 0 -> IO.return (Error Zero_progress)
      | Ok written when written < 0 || written > length ->
          IO.return (Error (Invalid_write_count written))
      | Ok written -> write_all file bytes (offset + written) limit

  let ensure_file t path =
    match t.current_file with
    | Some (current, file) when String.equal current path -> IO.return (Ok file)
    | Some _ -> (
        let* flushed = flush_current t in
        match flushed with
        | Error error -> IO.return (Error error)
        | Ok () -> (
            let* closed = close_current t in
            match closed with
            | Error error -> IO.return (Error error)
            | Ok () -> (
                let* opened = attempt (fun () -> IO.open_append path) in
                match opened with
                | Error error -> IO.return (Error error)
                | Ok file ->
                    t.current_file <- Some (path, file);
                    IO.return (Ok file))))
    | None -> (
        let* opened = attempt (fun () -> IO.open_append path) in
        match opened with
        | Error error -> IO.return (Error error)
        | Ok file ->
            t.current_file <- Some (path, file);
            IO.return (Ok file))

  let dispose t =
    IO.dispose t.worker_notifier;
    IO.dispose t.progress_notifier

  let fail t error =
    let first =
      IO.with_lock t.lock (fun () ->
          match t.status with
          | Failed _ | Stopped -> false
          | Running | Closing ->
              t.status <- Failed error;
              t.terminal_error <- Some error;
              Queue.clear t.queue;
              true)
    in
    if not first then IO.return ()
    else (
      Observe.Drain.Integration.report_failure ();
      IO.notify t.progress_notifier;
      IO.notify t.worker_notifier;
      let* _ = close_current t in
      dispose t;
      IO.return ())

  type action =
    | Write of write
    | Flush of int64
    | Wait of unit IO.t
    | Close
    | Stop

  let earliest_flush_barrier t =
    List.fold_left
      (fun earliest target ->
        if Int64.compare target earliest < 0 then target else earliest)
      Int64.max_int t.flush_barriers

  let coalesce_write t (first : record) =
    let path = first.path in
    let limit = earliest_flush_barrier t in
    let length = ref (append t.write_buffer 0 first.bytes) in
    let through = ref first.sequence in
    let taking = ref true in
    while !taking do
      match Queue.peek_opt t.queue with
      | Some next
        when String.equal next.path path
             && Int64.compare next.sequence limit <= 0
             && String.length next.bytes <= Sys.max_string_length - !length ->
          ignore (Queue.take t.queue);
          length := append t.write_buffer !length next.bytes;
          through := next.sequence
      | Some _ | None -> taking := false
    done;
    { path; bytes = t.write_buffer.view; length = !length; through = !through }

  let next_action t =
    IO.with_lock t.lock (fun () ->
        let ready_flush =
          match
            List.filter
              (fun target ->
                Int64.compare target t.flushed_sequence > 0
                && Int64.compare target t.completed_sequence <= 0)
              t.flush_barriers
            |> List.sort_uniq Int64.compare
          with
          | [] -> None
          | sequence :: _ -> Some sequence
        in
        match t.status with
        | Failed _ | Stopped -> Stop
        | Running | Closing -> (
            match ready_flush with
            | Some sequence -> Flush sequence
            | None -> (
                match Queue.take_opt t.queue with
                | Some record -> Write (coalesce_write t record)
                | None -> (
                    match t.status with
                    | Closing -> Close
                    | Running ->
                        t.worker_waiting <- true;
                        Wait (IO.await t.worker_notifier)
                    | Failed _ | Stopped -> assert false))))

  let complete_write t sequence =
    IO.with_lock t.lock (fun () -> t.completed_sequence <- sequence)

  let complete_flush t sequence =
    IO.with_lock t.lock (fun () ->
        t.flushed_sequence <- sequence;
        t.flush_barriers <-
          List.filter
            (fun target -> Int64.compare target sequence > 0)
            t.flush_barriers);
    IO.notify t.progress_notifier

  let complete_shutdown t =
    IO.with_lock t.lock (fun () -> t.status <- Stopped);
    IO.notify t.progress_notifier;
    dispose t

  let rec worker t =
    match next_action t with
    | Write write -> (
        let* ready = ensure_file t write.path in
        match ready with
        | Error error -> fail t error
        | Ok file -> (
            let* written = write_all file write.bytes 0 write.length in
            match written with
            | Error error -> fail t error
            | Ok () ->
                complete_write t write.through;
                worker t))
    | Flush sequence -> (
        let* flushed = flush_current t in
        match flushed with
        | Error error -> fail t error
        | Ok () ->
            complete_flush t sequence;
            worker t)
    | Wait promise ->
        let* () = promise in
        worker t
    | Close -> (
        let* closed = close_current t in
        match closed with
        | Error error -> fail t error
        | Ok () ->
            complete_shutdown t;
            IO.return ())
    | Stop -> IO.return ()

  let wake_worker t =
    let should_notify =
      IO.with_lock t.lock (fun () ->
          if t.worker_waiting then (
            t.worker_waiting <- false;
            true)
          else false)
    in
    if should_notify then IO.notify t.worker_notifier

  let formatter = Observe.Formatter.ndjson

  let offer t log =
    let projected =
      match Observe.Formatter.format t.formatter log with
      | Error _ -> None
      | Ok bytes ->
          let path =
            IO.child t.path
              (Observe_fs_date.Date.filename (Observe.Log.timestamp log))
          in
          Some (path, bytes)
      | exception exn ->
          preserve_fatal exn;
          None
    in
    match projected with
    | None -> Observe.Drain.Rejected
    | Some (path, bytes) ->
        let accepted, should_notify =
          IO.with_lock t.lock (fun () ->
              match t.status with
              | Closing | Failed _ | Stopped -> (false, false)
              | Running when Queue.length t.queue >= t.capacity -> (false, false)
              | Running ->
                  t.accepted_sequence <- Int64.succ t.accepted_sequence;
                  Queue.add
                    { sequence = t.accepted_sequence; path; bytes }
                    t.queue;
                  let should_notify = t.worker_waiting in
                  t.worker_waiting <- false;
                  (true, should_notify))
        in
        if accepted then (
          if should_notify then IO.notify t.worker_notifier;
          Observe.Drain.Accepted)
        else Observe.Drain.Rejected

  let drain t = Observe.Drain.create (offer t)

  let rec flush_through t target =
    let state =
      IO.with_lock t.lock (fun () ->
          match (t.status, t.terminal_error) with
          | Failed error, _ -> `Error error
          | _, Some error -> `Error error
          | Stopped, None when Int64.compare t.flushed_sequence target >= 0 ->
              `Done
          | Stopped, None -> `Done
          | (Running | Closing), None
            when Int64.compare t.flushed_sequence target >= 0 ->
              `Done
          | (Running | Closing), None ->
              if not (List.exists (Int64.equal target) t.flush_barriers) then
                t.flush_barriers <- target :: t.flush_barriers;
              `Wait (IO.await t.progress_notifier))
    in
    match state with
    | `Done -> IO.return (Ok ())
    | `Error error -> IO.return (Error error)
    | `Wait promise ->
        wake_worker t;
        let* () = promise in
        flush_through t target

  let flush t =
    let target = IO.with_lock t.lock (fun () -> t.accepted_sequence) in
    flush_through t target

  let rec wait_for_shutdown t =
    let state =
      IO.with_lock t.lock (fun () ->
          match (t.status, t.terminal_error) with
          | Failed error, _ -> `Error error
          | _, Some error -> `Error error
          | Stopped, None -> `Done
          | Running, None ->
              t.status <- Closing;
              if
                Int64.compare t.accepted_sequence t.flushed_sequence > 0
                && not
                     (List.exists
                        (Int64.equal t.accepted_sequence)
                        t.flush_barriers)
              then t.flush_barriers <- t.accepted_sequence :: t.flush_barriers;
              `Wait (IO.await t.progress_notifier)
          | Closing, None -> `Wait (IO.await t.progress_notifier))
    in
    match state with
    | `Done -> IO.return (Ok ())
    | `Error error -> IO.return (Error error)
    | `Wait promise ->
        wake_worker t;
        let* () = promise in
        wait_for_shutdown t

  let shutdown = wait_for_shutdown

  let create ~path ?(capacity = 1_024) () =
    if String.length path = 0 || contains_nul path then
      IO.return (Error Invalid_path)
    else if capacity <= 0 then IO.return (Error (Invalid_capacity capacity))
    else
      let* setup = attempt (fun () -> IO.ensure_directory path) in
      match setup with
      | Error error -> IO.return (Error error)
      | Ok () ->
          let t =
            {
              path;
              capacity;
              lock = IO.create_lock ();
              worker_notifier = IO.create_notifier ();
              progress_notifier = IO.create_notifier ();
              queue = Queue.create ();
              formatter;
              accepted_sequence = 0L;
              completed_sequence = 0L;
              flush_barriers = [];
              flushed_sequence = 0L;
              status = Running;
              terminal_error = None;
              current_file = None;
              worker_waiting = false;
              write_buffer = create_write_buffer ();
            }
          in
          IO.async (fun () ->
              IO.catch
                (fun () -> worker t)
                (fun exn ->
                  preserve_fatal exn;
                  fail t (Unexpected exn)));
          IO.return (Ok t)
end
