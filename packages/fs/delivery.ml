module Make (IO : Io.S) = struct
  type error =
    | Invalid_path
    | Invalid_capacity of int
    | Io of IO.error
    | Zero_progress
    | Invalid_write_count of int
    | Unexpected of exn

  type record = { sequence : int64; path : string; bytes : string }
  type status = Running | Closing | Failed of error | Stopped

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
    mutable flush_targets : int64 list;
    mutable flushed_sequence : int64;
    mutable status : status;
    mutable terminal_error : error option;
    mutable current : (string * IO.file) option;
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

  let attempt callback =
    IO.catch
      (fun () ->
        let* outcome = callback () in
        IO.return (Result.map_error (fun error -> Io error) outcome))
      (fun exn ->
        preserve_fatal exn;
        IO.return (Error (Unexpected exn)))

  let close_current t =
    match t.current with
    | None -> IO.return (Ok ())
    | Some (_, file) ->
        let* outcome = attempt (fun () -> IO.close file) in
        (match outcome with Ok () -> t.current <- None | Error _ -> ());
        IO.return outcome

  let flush_current t =
    match t.current with
    | None -> IO.return (Ok ())
    | Some (_, file) -> attempt (fun () -> IO.flush file)

  let rec write_all file bytes offset =
    let length = String.length bytes - offset in
    if length = 0 then IO.return (Ok ())
    else
      let* outcome = attempt (fun () -> IO.write file bytes ~offset ~length) in
      match outcome with
      | Error error -> IO.return (Error error)
      | Ok 0 -> IO.return (Error Zero_progress)
      | Ok written when written < 0 || written > length ->
          IO.return (Error (Invalid_write_count written))
      | Ok written -> write_all file bytes (offset + written)

  let ensure_file t path =
    match t.current with
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
                    t.current <- Some (path, file);
                    IO.return (Ok file))))
    | None -> (
        let* opened = attempt (fun () -> IO.open_append path) in
        match opened with
        | Error error -> IO.return (Error error)
        | Ok file ->
            t.current <- Some (path, file);
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
    | Record of record
    | Flush of int64
    | Wait of unit IO.t
    | Close
    | Stop

  let next_action t =
    IO.with_lock t.lock (fun () ->
        let ready_flush =
          match
            List.filter
              (fun target ->
                Int64.compare target t.flushed_sequence > 0
                && Int64.compare target t.completed_sequence <= 0)
              t.flush_targets
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
                | Some record -> Record record
                | None -> (
                    match t.status with
                    | Closing -> Close
                    | Running -> Wait (IO.await t.worker_notifier)
                    | Failed _ | Stopped -> assert false))))

  let complete_record t sequence =
    IO.with_lock t.lock (fun () -> t.completed_sequence <- sequence);
    IO.notify t.progress_notifier

  let complete_flush t sequence =
    IO.with_lock t.lock (fun () ->
        t.flushed_sequence <- sequence;
        t.flush_targets <-
          List.filter
            (fun target -> Int64.compare target sequence > 0)
            t.flush_targets);
    IO.notify t.progress_notifier

  let complete_shutdown t =
    IO.with_lock t.lock (fun () -> t.status <- Stopped);
    IO.notify t.progress_notifier;
    dispose t

  let rec worker t =
    match next_action t with
    | Record record -> (
        let* ready = ensure_file t record.path in
        match ready with
        | Error error -> fail t error
        | Ok file -> (
            let* written = write_all file record.bytes 0 in
            match written with
            | Error error -> fail t error
            | Ok () ->
                complete_record t record.sequence;
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
        let accepted =
          IO.with_lock t.lock (fun () ->
              match t.status with
              | Closing | Failed _ | Stopped -> false
              | Running when Queue.length t.queue >= t.capacity -> false
              | Running ->
                  t.accepted_sequence <- Int64.succ t.accepted_sequence;
                  Queue.add
                    { sequence = t.accepted_sequence; path; bytes }
                    t.queue;
                  true)
        in
        if accepted then (
          IO.notify t.worker_notifier;
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
              if not (List.exists (Int64.equal target) t.flush_targets) then
                t.flush_targets <- target :: t.flush_targets;
              `Wait (IO.await t.progress_notifier))
    in
    match state with
    | `Done -> IO.return (Ok ())
    | `Error error -> IO.return (Error error)
    | `Wait promise ->
        IO.notify t.worker_notifier;
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
                        t.flush_targets)
              then t.flush_targets <- t.accepted_sequence :: t.flush_targets;
              `Wait (IO.await t.progress_notifier)
          | Closing, None -> `Wait (IO.await t.progress_notifier))
    in
    match state with
    | `Done -> IO.return (Ok ())
    | `Error error -> IO.return (Error error)
    | `Wait promise ->
        IO.notify t.worker_notifier;
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
              flush_targets = [];
              flushed_sequence = 0L;
              status = Running;
              terminal_error = None;
              current = None;
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
