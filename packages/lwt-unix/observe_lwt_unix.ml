module Clock = struct
  let nanoseconds_per_day = 86_400_000_000_000L
  let picoseconds_per_nanosecond = 1_000L

  let timestamp_of_day_and_picoseconds (days, picoseconds) =
    let days = Int64.of_int days in
    if
      Int64.compare days 0L < 0
      || Int64.compare picoseconds 0L < 0
      || Int64.compare days (Int64.div Int64.max_int nanoseconds_per_day) > 0
    then Error Observe.IO.Unavailable
    else
      let day_nanoseconds = Int64.mul days nanoseconds_per_day in
      let subday_nanoseconds =
        Int64.div picoseconds picoseconds_per_nanosecond
      in
      if
        Int64.compare subday_nanoseconds
          (Int64.sub Int64.max_int day_nanoseconds)
        > 0
      then Error Observe.IO.Unavailable
      else
        Ok
          (Observe.Timestamp.of_unix_ns
             (Int64.add day_nanoseconds subday_nanoseconds))

  let now () = timestamp_of_day_and_picoseconds (Ptime_clock.now_d_ps ())
  let monotonic_now () = Ok (Mtime_clock.elapsed_ns ())
end

module Serialized_callback = struct
  let wrap callback =
    let mutex = Mutex.create () in
    let available = Condition.create () in
    let owner = ref None in
    fun () ->
      let thread = Thread.id (Thread.self ()) in
      Mutex.lock mutex;
      let rec acquire () =
        match !owner with
        | None -> owner := Some thread
        | Some active when active = thread ->
            Mutex.unlock mutex;
            invalid_arg "Observe_lwt_unix: reentrant serialized callback"
        | Some _ ->
            Condition.wait available mutex;
            acquire ()
      in
      acquire ();
      Mutex.unlock mutex;
      Fun.protect
        ~finally:(fun () ->
          Mutex.lock mutex;
          owner := None;
          Condition.broadcast available;
          Mutex.unlock mutex)
        callback
end

module One_shot_callback = struct
  type 'a outcome = Returned of 'a | Raised of exn * Printexc.raw_backtrace
  type 'a state = Pending | Running of int | Resolved of 'a outcome

  type 'a t = {
    callback : unit -> 'a;
    mutex : Mutex.t;
    available : Condition.t;
    state : 'a state Atomic.t;
  }

  let create callback =
    {
      callback;
      mutex = Mutex.create ();
      available = Condition.create ();
      state = Atomic.make Pending;
    }

  let replay = function
    | Returned value -> value
    | Raised (raised, backtrace) ->
        Printexc.raise_with_backtrace raised backtrace

  let rec get t =
    match Atomic.get t.state with
    | Resolved outcome -> replay outcome
    | Running owner ->
        if owner = Thread.id (Thread.self ()) then
          invalid_arg "Observe_lwt_unix: reentrant one-shot callback"
        else (
          Mutex.lock t.mutex;
          while
            match Atomic.get t.state with
            | Running _ -> true
            | Pending | Resolved _ -> false
          do
            Condition.wait t.available t.mutex
          done;
          Mutex.unlock t.mutex;
          get t)
    | Pending ->
        let thread = Thread.id (Thread.self ()) in
        if not (Atomic.compare_and_set t.state Pending (Running thread)) then
          get t
        else
          let outcome =
            match t.callback () with
            | value -> Returned value
            | exception raised -> Raised (raised, Printexc.get_raw_backtrace ())
          in
          Mutex.lock t.mutex;
          Atomic.set t.state (Resolved outcome);
          Condition.broadcast t.available;
          Mutex.unlock t.mutex;
          replay outcome
end

module Identity = struct
  type generator = unit -> string
  type t = { selected : generator Atomic.t }

  let default () =
    Mirage_crypto_rng_unix.getrandom 16
    |> Bytes.of_string
    |> Uuidm.v4
    |> Uuidm.to_string

  let create () = { selected = Atomic.make default }
  let get t = Atomic.get t.selected
  let set t generator = Atomic.set t.selected generator
  let serialize = Serialized_callback.wrap
  let next t () = Ok ((get t) ())
end

type id_generator = Identity.generator

module Sampling = struct
  type draw = unit -> float
  type t = { selected : draw Atomic.t }

  let seed () =
    let bytes = Mirage_crypto_rng_unix.getrandom 8 in
    let value = ref 0L in
    for index = 0 to 7 do
      value :=
        Int64.logor !value
          (Int64.shift_left
             (Int64.of_int (Char.code (String.unsafe_get bytes index)))
             (index * 8))
    done;
    !value

  let mix value =
    let value =
      Int64.mul
        (Int64.logxor value (Int64.shift_right_logical value 30))
        (-4658895280553007687L)
    in
    let value =
      Int64.mul
        (Int64.logxor value (Int64.shift_right_logical value 27))
        (-7723592293110705685L)
    in
    Int64.logxor value (Int64.shift_right_logical value 31)

  let create_default () =
    let initial =
      match seed () with
      | initial -> Ok initial
      | exception ((Out_of_memory | Stack_overflow | Sys.Break) as raised) ->
          raise raised
      | exception raised -> Error (raised, Printexc.get_raw_backtrace ())
    in
    let shard_count = 64 in
    let counters = Array.init shard_count (fun _ -> Atomic.make 0) in
    fun () ->
      match initial with
      | Error (raised, backtrace) ->
          Printexc.raise_with_backtrace raised backtrace
      | Ok initial ->
          let shard = Thread.id (Thread.self ()) land (shard_count - 1) in
          let offset = Atomic.fetch_and_add counters.(shard) 1 in
          let position =
            Int64.add
              (Int64.mul (Int64.of_int offset) (Int64.of_int shard_count))
              (Int64.of_int shard)
          in
          let value =
            Int64.add initial (Int64.mul position (-7046029254386353131L))
            |> mix
          in
          let mantissa = Int64.shift_right_logical value 11 in
          Int64.to_float mantissa /. 9_007_199_254_740_992.

  let default () =
    let source = One_shot_callback.create create_default in
    fun () -> (One_shot_callback.get source) ()

  let unavailable () = nan
  let create () = { selected = Atomic.make unavailable }
  let get t = Atomic.get t.selected
  let set t draw = Atomic.set t.selected draw
  let serialize = Serialized_callback.wrap
  let draw t () = (get t) ()

  let create_stable t () =
    let stable = One_shot_callback.create (get t) in
    fun () -> One_shot_callback.get stable
end

type sampling_draw = Sampling.draw

module Console = struct
  let lowercase = String.lowercase_ascii

  let contains ~needle haystack =
    let needle_length = String.length needle in
    let haystack_length = String.length haystack in
    let rec search offset =
      offset + needle_length <= haystack_length
      &&
      let rec matches index =
        index = needle_length
        || String.unsafe_get haystack (offset + index)
           = String.unsafe_get needle index
           && matches (index + 1)
      in
      matches 0 || search (offset + 1)
    in
    needle_length = 0 || search 0

  let capable_style ~term =
    match Option.map lowercase (Sys.getenv_opt "COLORTERM") with
    | Some ("truecolor" | "24bit") -> Observe.Formatter.Truecolor
    | Some _ | None ->
        if
          String.equal term "xterm-ghostty"
          || contains ~needle:"truecolor" term
          || contains ~needle:"-direct" term
        then Observe.Formatter.Truecolor
        else if contains ~needle:"256color" term then Observe.Formatter.Ansi_256
        else Observe.Formatter.Ansi_16

  let style () =
    try
      if not (Unix.isatty Unix.stderr) then Observe.Formatter.Plain
      else if Option.is_some (Sys.getenv_opt "NO_COLOR") then
        Observe.Formatter.Plain
      else
        match Option.map lowercase (Sys.getenv_opt "TERM") with
        | None | Some "" | Some "dumb" -> Observe.Formatter.Plain
        | Some term -> capable_style ~term
    with
    | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> raise exn
    | _ -> Observe.Formatter.Plain
end

module Observer = Observe.Make (Observe_lwt.IO)

type hook = {
  identity : Writer_registry.identity;
  flush : unit -> unit Lwt.t;
  shutdown : unit -> unit Lwt.t;
}

type context = {
  identity : Identity.t;
  sampling : Sampling.t;
  owner_thread : int Atomic.t;
  console : Writer.t option Atomic.t;
  observer : Observer.t;
}

type runtime = { context : context; writers : Writer_registry.t }

type lifecycle =
  | Fresh of hook list
  | Prepared of context * hook list
  | Running of runtime
  | Closing of context * unit Lwt.t
  | Closed of context * (unit, exn) result

let lifecycle = ref (Fresh [])
let lifecycle_lock = Mutex.create ()

let with_lifecycle callback =
  Mutex.lock lifecycle_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock lifecycle_lock) callback

let create_hook ~flush ~shutdown =
  { identity = Writer_registry.create_identity (); flush; shutdown }

let create_context () =
  let identity = Identity.create () in
  let sampling = Sampling.create () in
  let owner_thread = Atomic.make (Thread.id (Thread.self ())) in
  let console = Atomic.make None in
  let io =
    Observe_lwt.create ~clock:Clock.now ~monotonic_now:Clock.monotonic_now
      ~next_id:(Identity.next identity) ~console_style:Console.style
      ~sampling_draw:(Sampling.draw sampling)
      ~create_stable_sampling_draw:(Sampling.create_stable sampling)
      ~offer_console:(fun output ->
        match Atomic.get console with
        | None -> Observe.IO.Rejected
        | Some writer -> (
            match Writer.offer writer output with
            | Accepted -> Observe.IO.Accepted
            | Full | Closed -> Observe.IO.Rejected))
      ~can_lookup_context:(fun () ->
        Thread.id (Thread.self ()) = Atomic.get owner_thread)
      ()
  in
  { identity; sampling; owner_thread; console; observer = Observer.create io }

let register_hook writers (hook : hook) =
  match
    Writer_registry.register writers ~identity:hook.identity ~flush:hook.flush
      ~shutdown:hook.shutdown
  with
  | Ok () -> ()
  | Error Closed -> assert false

let create_registry ?writer pending =
  let writers = Writer_registry.create () in
  Option.iter
    (fun writer ->
      register_hook writers
        (create_hook
           ~flush:(fun () -> Writer.flush writer)
           ~shutdown:(fun () -> Writer.shutdown writer)))
    writer;
  List.iter (register_hook writers) (List.rev pending);
  writers

let get_or_create_context () =
  with_lifecycle (fun () ->
      match !lifecycle with
      | Fresh pending ->
          let context = create_context () in
          lifecycle := Prepared (context, pending);
          Some context
      | Prepared (context, _) | Running { context; _ } -> Some context
      | Closing _ | Closed _ -> None)

let init ?id_generator ?sampling_draw config =
  with_lifecycle (fun () ->
      match !lifecycle with
      | Running _ -> Error Observe.Already_initialized
      | Closing _ | Closed _ -> Error Observe.Runtime_closed
      | (Fresh _ | Prepared _) as before -> (
          let context, pending =
            match before with
            | Fresh pending -> (create_context (), pending)
            | Prepared (context, pending) -> (context, pending)
            | Running _ | Closing _ | Closed _ -> assert false
          in
          lifecycle := Prepared (context, pending);
          let previous_identity = Identity.get context.identity in
          let previous_sampling = Sampling.get context.sampling in
          let previous_owner_thread = Atomic.get context.owner_thread in
          let selected_identity =
            Option.fold ~none:Identity.default ~some:Identity.serialize
              id_generator
          in
          Identity.set context.identity selected_identity;
          let selected_sampling =
            match (sampling_draw, Observe.Config.sampling config) with
            | Some draw, Some _ -> Sampling.serialize draw
            | None, Some _ -> Sampling.default ()
            | _, None -> Sampling.unavailable
          in
          Sampling.set context.sampling selected_sampling;
          Atomic.set context.owner_thread (Thread.id (Thread.self ()));
          let writer =
            try
              match
                (Observe.Config.enabled config, Observe.Config.console config)
              with
              | false, _ | true, Observe.Config.Silent -> None
              | true, (Observe.Config.Auto | Observe.Config.Pretty)
              | true, Observe.Config.Ndjson ->
                  Some (Writer.create ~capacity:1_024 Lwt_unix.stderr)
            with raised ->
              Identity.set context.identity previous_identity;
              Sampling.set context.sampling previous_sampling;
              Atomic.set context.owner_thread previous_owner_thread;
              lifecycle := Prepared (context, pending);
              raise raised
          in
          Atomic.set context.console writer;
          let rollback () =
            Atomic.set context.console None;
            Identity.set context.identity previous_identity;
            Sampling.set context.sampling previous_sampling;
            Atomic.set context.owner_thread previous_owner_thread;
            Option.iter Writer.abort writer;
            lifecycle := Prepared (context, pending)
          in
          match create_registry ?writer pending with
          | exception raised ->
              rollback ();
              raise raised
          | writers -> (
              match Observer.init context.observer config with
              | exception raised ->
                  rollback ();
                  raise raised
              | Ok () ->
                  lifecycle := Running { context; writers };
                  Ok ()
              | Error _ as error ->
                  rollback ();
                  error)))

let init_exn ?id_generator ?sampling_draw config =
  match init ?id_generator ?sampling_draw config with
  | Ok () -> ()
  | Error error -> raise (Observe.Init_error error)

let with_operation ~name ?using ?error callback =
  match get_or_create_context () with
  | Some context ->
      Observer.with_operation context.observer ~name ?using ?error callback
  | None -> callback ()

let fork ~name ?using ?error callback =
  match get_or_create_context () with
  | Some context -> Observer.fork context.observer ~name ?using ?error callback
  | None -> raise (Observe.Logs.Current_error Observe.Logs.Not_bound)

let result_promise = function
  | Ok () -> Lwt.return_unit
  | Error raised -> Lwt.fail raised

let flush () =
  let action =
    with_lifecycle (fun () ->
        match !lifecycle with
        | Fresh pending | Prepared (_, pending) -> `Pending pending
        | Running runtime -> `Registry runtime.writers
        | Closing (_, promise) -> `Promise promise
        | Closed (_, outcome) -> `Outcome outcome)
  in
  match action with
  | `Pending pending -> Writer_registry.flush (create_registry pending)
  | `Registry writers -> Writer_registry.flush writers
  | `Promise promise -> Lwt.protected promise
  | `Outcome outcome -> result_promise outcome

let settle_shutdown context promise wakener work =
  Lwt.on_any work
    (fun () ->
      with_lifecycle (fun () -> lifecycle := Closed (context, Ok ()));
      Lwt.wakeup_later wakener ())
    (fun raised ->
      with_lifecycle (fun () -> lifecycle := Closed (context, Error raised));
      Lwt.wakeup_later_exn wakener raised);
  Lwt.protected promise

let shutdown () =
  let action =
    with_lifecycle (fun () ->
        match !lifecycle with
        | Closing (_, promise) -> `Promise promise
        | Closed (_, outcome) -> `Outcome outcome
        | Fresh pending ->
            let context = create_context () in
            Observer.close context.observer;
            let promise, wakener = Lwt.wait () in
            lifecycle := Closing (context, promise);
            `Start (context, create_registry pending, promise, wakener)
        | Prepared (context, pending) ->
            Observer.close context.observer;
            let promise, wakener = Lwt.wait () in
            lifecycle := Closing (context, promise);
            `Start (context, create_registry pending, promise, wakener)
        | Running runtime ->
            Observer.close runtime.context.observer;
            let promise, wakener = Lwt.wait () in
            lifecycle := Closing (runtime.context, promise);
            `Start (runtime.context, runtime.writers, promise, wakener))
  in
  match action with
  | `Promise promise -> Lwt.protected promise
  | `Outcome outcome -> result_promise outcome
  | `Start (context, writers, promise, wakener) ->
      settle_shutdown context promise wakener (Writer_registry.shutdown writers)

module Lifecycle = struct
  type error = Writer_registry.error = Closed

  let register ~flush ~shutdown =
    let hook = create_hook ~flush ~shutdown in
    with_lifecycle (fun () ->
        match !lifecycle with
        | Fresh pending ->
            lifecycle := Fresh (hook :: pending);
            Ok ()
        | Prepared (context, pending) ->
            lifecycle := Prepared (context, hook :: pending);
            Ok ()
        | Running runtime ->
            Writer_registry.register runtime.writers ~identity:hook.identity
              ~flush:hook.flush ~shutdown:hook.shutdown
        | Closing _ | Closed _ -> Error Closed)
end

module Test = struct
  exception Capture_error of Observe.capture_error

  let with_capture_exn ~config ?capacity callback =
    match get_or_create_context () with
    | None -> Lwt.fail (Capture_error Observe.Runtime_closed)
    | Some context ->
        Lwt.bind
          (Observer.with_capture context.observer ~config ?capacity callback)
          (function
          | Ok result -> Lwt.return result
          | Error error -> Lwt.fail (Capture_error error))
end
