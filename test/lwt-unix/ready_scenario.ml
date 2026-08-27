let fail format = Format.kasprintf failwith format

let text ~tag message (builder : Observe.Logs.builder) =
  builder.text ~tag "%s" message

let check condition format =
  Format.kasprintf
    (fun message -> if not condition then failwith message)
    format

let contains value fragment =
  let value_length = String.length value in
  let fragment_length = String.length fragment in
  let rec loop index =
    if index + fragment_length > value_length then false
    else if String.sub value index fragment_length = fragment then true
    else loop (index + 1)
  in
  fragment_length = 0 || loop 0

let read_all descriptor =
  let bytes = Bytes.create 256 in
  let buffer = Buffer.create 256 in
  let rec loop () =
    match Unix.read descriptor bytes 0 (Bytes.length bytes) with
    | 0 -> Buffer.contents buffer
    | count ->
        Buffer.add_subbytes buffer bytes 0 count;
        loop ()
  in
  loop ()

let capture_stderr callback =
  let path = Filename.temp_file "observe" ".stderr" in
  let output = Unix.openfile path [ Unix.O_RDWR ] 0o600 in
  Sys.remove path;
  let saved = Unix.dup Unix.stderr in
  Unix.dup2 output Unix.stderr;
  let outcome =
    match callback () with
    | value ->
        Lwt_main.run (Observe_lwt_unix.flush ());
        Ok value
    | exception exn -> Error (exn, Printexc.get_raw_backtrace ())
  in
  Unix.dup2 saved Unix.stderr;
  Unix.close saved;
  ignore (Unix.lseek output 0 Unix.SEEK_SET : int);
  let captured = read_all output in
  Unix.close output;
  match outcome with
  | Ok value -> (value, captured)
  | Error (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace

let discard_stderr callback =
  let output = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  let saved = Unix.dup Unix.stderr in
  Unix.dup2 output Unix.stderr;
  Unix.close output;
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 saved Unix.stderr;
      Unix.close saved)
    callback

let config ?environment ?console ?drains ?sampling service =
  Observe.Config.create_exn ~service ?environment ?console ?drains ?sampling ()

let text_tag log =
  match Observe.Log.event log with
  | Observe.Log.Text { tag; _ } -> tag
  | Observe.Log.Structured _ -> fail "expected text log"

let capture_tags capture = List.map text_tag (Observe.Capture.logs capture)

let diagnostic_count capture kind =
  List.fold_left
    (fun total (entry : Observe.Diagnostics.entry) ->
      if entry.kind = kind then total + entry.count else total)
    0
    (Observe.Capture.diagnostics capture)

let process_diagnostic_count kind =
  List.fold_left
    (fun total (entry : Observe.Diagnostics.entry) ->
      if entry.kind = kind then total + entry.count else total)
    0
    (Observe.Diagnostics.snapshot ())

let wide_operation = function
  | log -> (
      match Observe.Log.kind log with
      | Observe.Log.Wide { operation; _ } -> operation
      | Observe.Log.Point _ -> fail "expected a wide operation")

let is_wide log =
  match Observe.Log.kind log with
  | Observe.Log.Wide _ -> true
  | Observe.Log.Point _ -> false

let operation_id log = Observe.Log.operation_id (wide_operation log)

let correlation_by_tag capture tag =
  Observe.Capture.logs capture
  |> List.find_map (fun log ->
      match Observe.Log.event log with
      | Observe.Log.Text { tag = actual; _ } when String.equal actual tag ->
          Some
            (match Observe.Log.kind log with
            | Observe.Log.Point { correlation } ->
                Option.map Observe.Log.operation_reference_id correlation
            | Observe.Log.Wide _ -> fail "expected point log")
      | Observe.Log.Text _ | Observe.Log.Structured _ -> None)
  |> Option.value ~default:None

let process_diagnostic_count kind =
  List.fold_left
    (fun total (entry : Observe.Diagnostics.entry) ->
      if entry.kind = kind then total + entry.count else total)
    0
    (Observe.Diagnostics.snapshot ())

let clock () =
  let captured = ref None in
  let drain =
    Observe.Drain.create (fun log ->
        captured := Some log;
        Observe.Drain.Accepted)
  in
  let before = Unix.gettimeofday () in
  Observe_lwt_unix.init_exn
    (config ~console:Observe.Config.Silent ~drains:[ drain ] "ready-clock");
  Observe.Logs.info (text ~tag:"clock" "sample");
  let after = Unix.gettimeofday () in
  let timestamp =
    match !captured with
    | Some log -> Observe.Log.timestamp log |> Observe.Timestamp.to_unix_ns
    | None -> fail "ready I/O did not deliver the clock sample"
  in
  let seconds = Int64.to_float timestamp /. 1_000_000_000.0 in
  check (seconds >= before -. 0.001) "clock sample preceded test bounds";
  check (seconds <= after +. 0.001) "clock sample exceeded test bounds"

let default_identity () =
  let captured = ref None in
  let drain =
    Observe.Drain.create (fun log ->
        captured := Some log;
        Observe.Drain.Accepted)
  in
  Observe_lwt_unix.init_exn
    (config ~console:Observe.Config.Silent ~drains:[ drain ] "default-id");
  let wide = Observe.Logs.create ~name:"identity" () in
  Observe.Logs.emit wide;
  let id =
    match !captured with
    | Some log -> operation_id log
    | None -> fail "default identity operation was not delivered"
  in
  match Uuidm.of_string id with
  | Some uuid ->
      check (Uuidm.version uuid = 4) "default identity was not UUID v4"
  | None -> fail "default identity was not a UUID: %S" id

let custom_identity () =
  let captured = ref [] in
  let captured_lock = Mutex.create () in
  let drain =
    Observe.Drain.create (fun log ->
        Mutex.lock captured_lock;
        captured := operation_id log :: !captured;
        Mutex.unlock captured_lock;
        Observe.Drain.Accepted)
  in
  let counter = ref 0 in
  let generating = Atomic.make false in
  let id_generator () =
    check
      (Atomic.compare_and_set generating false true)
      "custom identity generator was invoked concurrently";
    Fun.protect
      ~finally:(fun () -> Atomic.set generating false)
      (fun () ->
        let next = !counter + 1 in
        Thread.delay 0.001;
        counter := next;
        Format.sprintf "custom-operation-%d" next)
  in
  Observe_lwt_unix.init_exn ~id_generator
    (config ~console:Observe.Config.Silent ~drains:[ drain ] "custom-id");
  let threads =
    List.init 16 (fun _ ->
        Thread.create
          (fun () ->
            let wide = Observe.Logs.create ~name:"identity" () in
            Observe.Logs.emit wide)
          ())
  in
  List.iter Thread.join threads;
  let identities = List.sort_uniq String.compare !captured in
  check (List.length identities = 16) "custom identities were not fresh";
  check
    (List.mem "custom-operation-1" identities
    && List.mem "custom-operation-16" identities)
    "custom identity generator did not determine operation identities"

let failing_identity () =
  let delivered = ref 0 in
  let authored = ref 0 in
  let drain =
    Observe.Drain.create (fun _ ->
        incr delivered;
        Observe.Drain.Accepted)
  in
  Observe_lwt_unix.init_exn
    ~id_generator:(fun () -> failwith "identity source failed")
    (config ~console:Observe.Config.Silent ~drains:[ drain ] "failing-id");
  let wide = Observe.Logs.create ~name:"identity" () in
  Observe.Logs.set wide (fun builder ->
      incr authored;
      builder.seal builder.untyped);
  Observe.Logs.emit wide;
  check (!authored = 0) "failed identity did not leave an inert handle";
  check (!delivered = 0) "failed identity published an operation";
  check
    (process_diagnostic_count Observe.Diagnostics.Identity_raised = 1)
    "failed identity was not contained and diagnosed"

let console () =
  let (), output =
    capture_stderr (fun () ->
        Observe_lwt_unix.init_exn (config ~environment:"development" "ready");
        Observe.Logs.info (text ~tag:"startup" "service ready"))
  in
  check
    (contains output " INFO [startup] service ready\n")
    "unexpected pretty output: %S" output;
  check
    (not (contains output "\027["))
    "redirected console output contained ANSI styling: %S" output;
  check
    (String.fold_left
       (fun count character -> if character = '\n' then count + 1 else count)
       0 output
    = 1)
    "expected one console record: %S" output

let json_console () =
  let (), output =
    capture_stderr (fun () ->
        Observe_lwt_unix.init_exn
          (config ~environment:"development" ~console:Observe.Config.Ndjson
             "ready-json");
        Observe.Logs.info (text ~tag:"json" "structured"))
  in
  check
    (contains output "\"service\":\"ready-json\"")
    "service missing from JSON";
  check (contains output "\"level\":\"info\"") "level missing from JSON"

let wide_json_console () =
  let (), output =
    capture_stderr (fun () ->
        Observe_lwt_unix.init_exn
          (config ~environment:"production" ~console:Observe.Config.Ndjson
             "ready-wide-json");
        let wide = Observe.Logs.create ~name:"checkout" () in
        Observe.Logs.set wide (fun m ->
            let open Observe.Logs in
            m.untyped
            |+ m.field "cart_id" Observe.Type.string "cart-1"
            |> m.seal);
        Observe.Logs.emit wide)
  in
  check
    (contains output "\"operation\":\"checkout\",\"operation_id\":")
    "wide operation fields missing from ready JSON: %S" output;
  check
    (contains output "\"duration_ms\":")
    "wide duration missing from ready JSON: %S" output;
  check
    (contains output "\"cart_id\":\"cart-1\"")
    "wide event fields missing from ready JSON: %S" output;
  check
    ((not (contains output "\"status\":"))
    && (not (contains output "\"outcome\":"))
    && not (contains output "\"requestLogs\":"))
    "ready JSON invented wide meaning: %S" output;
  check
    (String.fold_left
       (fun count character -> if character = '\n' then count + 1 else count)
       0 output
    = 1)
    "wide ready console emitted more than one NDJSON record: %S" output

let production_json_console () =
  let (), output =
    capture_stderr (fun () ->
        Observe_lwt_unix.init_exn
          (config ~environment:"production" "ready-production");
        Observe.Logs.info (text ~tag:"json" "structured"))
  in
  check
    (contains output "\"service\":\"ready-production\"")
    "production did not select JSON: %S" output

let repeated_init () =
  let config = config ~console:Observe.Config.Silent "repeat" in
  Observe_lwt_unix.init_exn config;
  check
    (Observe_lwt_unix.init config = Error Observe.Already_initialized)
    "second initialization was not rejected";
  match Observe_lwt_unix.init_exn config with
  | exception Observe.Init_error Observe.Already_initialized -> ()
  | exception exn ->
      fail "unexpected initialization exception: %s" (Printexc.to_string exn)
  | () -> fail "second init_exn unexpectedly succeeded"

let custom_sampling () =
  let delivered = Atomic.make 0 in
  let drain =
    Observe.Drain.create (fun _ ->
        ignore (Atomic.fetch_and_add delivered 1 : int);
        Observe.Drain.Accepted)
  in
  let active = Atomic.make false in
  let calls = Atomic.make 0 in
  let sampling_draw () =
    if not (Atomic.compare_and_set active false true) then
      failwith "sampling source invoked concurrently";
    Fun.protect
      ~finally:(fun () -> Atomic.set active false)
      (fun () ->
        let call = Atomic.fetch_and_add calls 1 in
        if call mod 2 = 0 then 0.25 else 0.75)
  in
  let sampling =
    Observe.Logs.Sampling.create
      ~info:(Observe.Logs.Sampling.Rate.percent_exn 50.)
      ()
  in
  Observe_lwt_unix.init_exn ~sampling_draw
    (config ~console:Observe.Config.Silent ~drains:[ drain ] ~sampling
       "sampling");
  let threads =
    List.init 16 (fun _ ->
        Thread.create
          (fun () -> Observe.Logs.info (text ~tag:"sample" "value"))
          ())
  in
  List.iter Thread.join threads;
  check (Atomic.get calls = 16) "custom sampling source call count changed";
  check (Atomic.get delivered = 8) "custom sampling decisions were not used"

let unused_custom_sampling () =
  let calls = ref 0 in
  let delivered = ref 0 in
  let drain =
    Observe.Drain.create (fun _ ->
        incr delivered;
        Observe.Drain.Accepted)
  in
  Observe_lwt_unix.init_exn
    ~sampling_draw:(fun () ->
      incr calls;
      0.99)
    (config ~console:Observe.Config.Silent ~drains:[ drain ]
       ~sampling:(Observe.Logs.Sampling.create ())
       "unused-sampling");
  Observe.Logs.info (text ~tag:"sample" "value");
  check (!calls = 0) "exact sampling invoked its source";
  check (!delivered = 1) "inert configured sampling changed retention"

let stable_custom_sampling () =
  let thread_count = 16 in
  let started = Atomic.make 0 in
  let release = Atomic.make false in
  let calls = Atomic.make 0 in
  let delivered = Atomic.make 0 in
  let drain =
    Observe.Drain.create (fun _ ->
        ignore (Atomic.fetch_and_add delivered 1 : int);
        Observe.Drain.Accepted)
  in
  let sampling_draw () =
    ignore (Atomic.fetch_and_add calls 1 : int);
    while not (Atomic.get release) do
      Thread.yield ()
    done;
    0.25
  in
  let sampling =
    Observe.Logs.Sampling.create
      ~info:(Observe.Logs.Sampling.Rate.percent_exn 50.)
      ~stability:Observe.Logs.Sampling.Correlation_stable ()
  in
  Observe_lwt_unix.init_exn ~sampling_draw
    (config ~console:Observe.Config.Silent ~drains:[ drain ] ~sampling
       "stable-sampling");
  let root = Observe.Logs.create ~name:"stable-root" () in
  let threads =
    List.init thread_count (fun _ ->
        Thread.create
          (fun () ->
            ignore (Atomic.fetch_and_add started 1 : int);
            Observe.Logs.info ~operation:root (text ~tag:"sample" "value"))
          ())
  in
  while Atomic.get started < thread_count do
    Thread.yield ()
  done;
  Atomic.set release true;
  List.iter Thread.join threads;
  Observe.Logs.emit root;
  check
    (Atomic.get calls = 1)
    "stable sampling invoked its source more than once";
  check
    (Atomic.get delivered = thread_count + 1)
    "stable sampling did not share one keep decision"

let reentrant_sampling () =
  let delivered = ref 0 in
  let entered = ref false in
  let drain =
    Observe.Drain.create (fun _ ->
        incr delivered;
        Observe.Drain.Accepted)
  in
  let sampling_draw () =
    if not !entered then (
      entered := true;
      Observe.Logs.info (text ~tag:"sample" "nested"));
    0.25
  in
  let sampling =
    Observe.Logs.Sampling.create
      ~info:(Observe.Logs.Sampling.Rate.percent_exn 50.)
      ()
  in
  Observe_lwt_unix.init_exn ~sampling_draw
    (config ~console:Observe.Config.Silent ~drains:[ drain ] ~sampling
       "reentrant-sampling");
  Observe.Logs.info (text ~tag:"sample" "outer");
  check (!delivered = 2) "reentrant sampling did not fail open";
  check
    (process_diagnostic_count Observe.Diagnostics.Sampling_source_raised = 1)
    "reentrant sampling was not diagnosed exactly once"

let init_rollback () =
  let module Conflict = Observe.Make (Observe_lwt.IO) in
  let state =
    Observe_lwt.create
      ~clock:(fun () -> Ok (Observe.Timestamp.of_unix_ns 0L))
      ~monotonic_now:(fun () -> Ok 0L)
      ~next_id:(fun () -> Ok "conflicting-operation")
      ~sampling_draw:(fun () -> 0.)
      ~create_stable_sampling_draw:(fun () -> fun () -> 0.)
      ~console_style:(fun () -> Observe.Formatter.Plain)
      ~offer_console:(fun _ -> Observe.IO.Accepted)
      ~can_lookup_context:(fun () -> true)
      ()
  in
  let conflict = Conflict.create state in
  Conflict.init_exn conflict
    (config ~console:Observe.Config.Silent "conflicting-runtime");
  let stopped = ref 0 in
  Observe_lwt_unix.Lifecycle.register
    ~flush:(fun () -> Lwt.return_unit)
    ~shutdown:(fun () ->
      incr stopped;
      Lwt.return_unit)
  |> Result.get_ok;
  let ready = config "rolled-back-runtime" in
  check
    (Observe_lwt_unix.init ready = Error Observe.IO_already_registered)
    "failed initialization did not report the conflicting runtime";
  check
    (Observe_lwt_unix.init ready = Error Observe.IO_already_registered)
    "failed initialization left the ready lifecycle initialized";
  Lwt_main.run (Observe_lwt_unix.shutdown ());
  check (!stopped = 1) "failed initialization lost its pending lifecycle hook"

let silent_drain () =
  let delivered = ref 0 in
  let drain =
    Observe.Drain.create (fun _ ->
        incr delivered;
        Observe.Drain.Accepted)
  in
  let (), output =
    capture_stderr (fun () ->
        Observe_lwt_unix.init_exn
          (config ~console:Observe.Config.Silent ~drains:[ drain ] "silent");
        Observe.Logs.info (text ~tag:"silent" "message"))
  in
  check (output = "") "silent logging wrote to stderr: %S" output;
  check (!delivered = 1) "silent logging skipped the configured drain"

let no_output () =
  Observe_lwt_unix.init_exn (config ~console:Observe.Config.Silent "no-output");
  check
    (process_diagnostic_count Observe.Diagnostics.No_delivery_target = 1)
    "no-output initialization was not diagnosed once"

let bounded_console () =
  discard_stderr (fun () ->
      Observe_lwt_unix.init_exn
        (config ~environment:"production" "bounded-console");
      for index = 0 to 1_024 do
        Observe.Logs.info (text ~tag:"bounded" (string_of_int index))
      done;
      check
        (process_diagnostic_count Observe.Diagnostics.Console_rejected = 1)
        "full console queue was not rejected exactly once";
      Lwt_main.run (Observe_lwt_unix.flush ()))

let serialized_console () =
  let (), output =
    capture_stderr (fun () ->
        Observe_lwt_unix.init_exn
          (config ~environment:"production" "serialized-console");
        for index = 0 to 99 do
          Observe.Logs.info (text ~tag:"serialized" (string_of_int index))
        done)
  in
  let lines = String.split_on_char '\n' output in
  let lines = List.filter (fun line -> String.length line > 0) lines in
  check (List.length lines = 100) "serialized record count changed";
  List.iter
    (fun line ->
      check
        (contains line "\"service\":\"serialized-console\"")
        "serialized output contained a partial or foreign record: %S" line)
    lines

let shutdown () =
  let delivered = ref 0 in
  let drain =
    Observe.Drain.create (fun _ ->
        incr delivered;
        Observe.Drain.Accepted)
  in
  let authored = ref 0 in
  let author message (m : Observe.Logs.builder) =
    incr authored;
    m.text ~tag:"shutdown" "%s" message
  in
  let (), output =
    capture_stderr (fun () ->
        Observe_lwt_unix.init_exn (config ~drains:[ drain ] "shutdown");
        Observe.Logs.info (author "before");
        Lwt_main.run (Observe_lwt_unix.shutdown ());
        Observe.Logs.info (author "after");
        check
          (process_diagnostic_count Observe.Diagnostics.Runtime_closed = 1)
          "post-shutdown logging was not diagnosed")
  in
  check (contains output "before") "shutdown lost an accepted record";
  check (not (contains output "after")) "shutdown accepted a later record";
  check (!authored = 1) "shutdown evaluated a later author callback";
  check (!delivered = 1) "shutdown delivered a later record to a drain";
  Lwt_main.run (Observe_lwt_unix.shutdown ())

let shutdown_before_init () =
  Lwt_main.run (Observe_lwt_unix.shutdown ());
  let config = config ~console:Observe.Config.Silent "closed-before-init" in
  check
    (Observe_lwt_unix.init config = Error Observe.Runtime_closed)
    "initialization succeeded after terminal shutdown";
  (match Observe_lwt_unix.init_exn config with
  | exception Observe.Init_error Observe.Runtime_closed -> ()
  | exception raised ->
      fail "unexpected post-shutdown init exception: %s"
        (Printexc.to_string raised)
  | () -> fail "init_exn succeeded after terminal shutdown");
  check
    (Observe_lwt_unix.Lifecycle.register
       ~flush:(fun () -> Lwt.return_unit)
       ~shutdown:(fun () -> Lwt.return_unit)
    = Error Observe_lwt_unix.Lifecycle.Closed)
    "closed lifecycle accepted a new output";
  let called = ref false in
  let capture =
    Observe_lwt_unix.Test.with_capture_exn ~config (fun _ ->
        called := true;
        Lwt.return_unit)
  in
  (match Lwt_main.run capture with
  | exception Observe_lwt_unix.Test.Capture_error Observe.Runtime_closed -> ()
  | exception raised ->
      fail "unexpected post-shutdown capture exception: %s"
        (Printexc.to_string raised)
  | () -> fail "capture succeeded after terminal shutdown");
  check (not !called) "closed capture evaluated its callback"

let prepared_shutdown () =
  Lwt_main.run
    (Observe_lwt_unix.with_operation ~name:"pre-init" (fun () ->
         Lwt.return_unit));
  Lwt_main.run (Observe_lwt_unix.shutdown ());
  let authored = ref false in
  Observe.Logs.info (fun m ->
      authored := true;
      m.text ~tag:"closed" "message");
  check (not !authored) "prepared shutdown evaluated a later author";
  check
    (process_diagnostic_count Observe.Diagnostics.Runtime_closed = 1)
    "prepared shutdown left the global route vacant";
  let called = ref false in
  let capture =
    Observe_lwt_unix.Test.with_capture_exn ~config:(config "closed-capture")
      (fun _ ->
        called := true;
        Lwt.return_unit)
  in
  (match Lwt_main.run capture with
  | exception Observe_lwt_unix.Test.Capture_error Observe.Runtime_closed -> ()
  | exception raised ->
      fail "unexpected prepared-shutdown capture exception: %s"
        (Printexc.to_string raised)
  | () -> fail "prepared-shutdown capture unexpectedly succeeded");
  check (not !called) "prepared-shutdown capture evaluated its callback"

let active_capture_survives_shutdown () =
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn ~config:(config "active-capture")
         (fun capture ->
           Observe_lwt_unix.init_exn
             (config ~console:Observe.Config.Silent "production");
           Observe.Logs.info (text ~tag:"before-shutdown" "message");
           Lwt.bind (Observe_lwt_unix.shutdown ()) (fun () ->
               Observe.Logs.info (text ~tag:"after-shutdown" "message");
               Lwt.return capture)))
  in
  check
    (capture_tags capture = [ "before-shutdown"; "after-shutdown" ])
    "production shutdown hid an active lexical capture"

let initialization_rebinds_owner () =
  let pre_init =
    Thread.create
      (fun () ->
        Lwt_main.run
          (Observe_lwt_unix.with_operation ~name:"foreign-pre-init" (fun () ->
               Lwt.return_unit)))
      ()
  in
  Thread.join pre_init;
  Observe_lwt_unix.init_exn
    (config ~console:Observe.Config.Silent "owner-rebind");
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn ~config:(config "owner-capture")
         (fun capture ->
           Lwt.bind
             (Observe_lwt_unix.with_operation ~name:"owner-parent" (fun () ->
                  Observe_lwt_unix.fork ~name:"owner-child" (fun () ->
                      Observe.Logs.info (text ~tag:"owner-child" "message");
                      Lwt.return_unit)))
             (fun () -> Lwt.return capture)))
  in
  check
    (Option.is_some (correlation_by_tag capture "owner-child"))
    "initialization retained the pre-init caller as context owner"

let lifecycle_flush () =
  let flushed = ref 0 in
  let stopped = ref 0 in
  let register () =
    Observe_lwt_unix.Lifecycle.register
      ~flush:(fun () ->
        incr flushed;
        Lwt.return_unit)
      ~shutdown:(fun () ->
        incr stopped;
        Lwt.return_unit)
    |> Result.get_ok
  in
  register ();
  register ();
  Lwt_main.run (Observe_lwt_unix.flush ());
  check (!flushed = 2) "flush did not visit every registered output";
  check (!stopped = 0) "flush ran shutdown hooks";
  Lwt_main.run (Observe_lwt_unix.shutdown ());
  check (!stopped = 2) "shutdown did not visit every registered output"

let lifecycle_init_transfer () =
  let flushed = ref 0 in
  let stopped = ref 0 in
  Observe_lwt_unix.Lifecycle.register
    ~flush:(fun () ->
      incr flushed;
      Lwt.return_unit)
    ~shutdown:(fun () ->
      incr stopped;
      Lwt.return_unit)
  |> Result.get_ok;
  Observe_lwt_unix.init_exn
    (config ~console:Observe.Config.Silent "lifecycle-init-transfer");
  Lwt_main.run (Observe_lwt_unix.flush ());
  check (!flushed = 1) "initialization lost a registered flush hook";
  Lwt_main.run (Observe_lwt_unix.shutdown ());
  check (!stopped = 1) "initialization lost a registered shutdown hook"

let lifecycle_failure () =
  let attempted = ref 0 in
  let flush_attempted = ref 0 in
  Observe_lwt_unix.Lifecycle.register
    ~flush:(fun () ->
      incr flush_attempted;
      Lwt.fail (Failure "flush"))
    ~shutdown:(fun () ->
      incr attempted;
      Lwt.fail (Failure "first"))
  |> Result.get_ok;
  Observe_lwt_unix.Lifecycle.register
    ~flush:(fun () ->
      incr flush_attempted;
      Lwt.return_unit)
    ~shutdown:(fun () ->
      incr attempted;
      Lwt.return_unit)
  |> Result.get_ok;
  let flush_outcome =
    Lwt_main.run
      (Lwt.catch
         (fun () -> Lwt.map Result.ok (Observe_lwt_unix.flush ()))
         (fun exn -> Lwt.return (Error exn)))
  in
  (match flush_outcome with
  | Error (Failure message) when String.equal message "flush" -> ()
  | Error exn -> fail "unexpected flush failure: %s" (Printexc.to_string exn)
  | Ok () -> fail "failing lifecycle hook did not fail flush");
  check (!flush_attempted = 2) "failure prevented another flush hook";
  let outcome =
    Lwt_main.run
      (Lwt.catch
         (fun () -> Lwt.map Result.ok (Observe_lwt_unix.shutdown ()))
         (fun exn -> Lwt.return (Error exn)))
  in
  (match outcome with
  | Error (Failure message) when String.equal message "first" -> ()
  | Error exn ->
      fail "unexpected lifecycle failure: %s" (Printexc.to_string exn)
  | Ok () -> fail "failing lifecycle hook did not fail shutdown");
  check (!attempted = 2) "failure prevented another shutdown hook";
  let repeated =
    Lwt_main.run
      (Lwt.catch
         (fun () -> Lwt.map Result.ok (Observe_lwt_unix.shutdown ()))
         (fun exn -> Lwt.return (Error exn)))
  in
  (match repeated with
  | Error (Failure message) when String.equal message "first" -> ()
  | Error exn ->
      fail "repeated shutdown changed failure: %s" (Printexc.to_string exn)
  | Ok () -> fail "repeated shutdown forgot the lifecycle failure");
  check (!attempted = 2) "repeated shutdown reran registered hooks"

let basic_capture () =
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn ~config:(config "capture")
         (fun capture ->
           Observe.Logs.info (text ~tag:"captured" "message");
           Lwt.return capture))
  in
  check (capture_tags capture = [ "captured" ]) "capture missed the log"

let operation_scope () =
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn ~config:(config "operation-scope")
         (fun capture ->
           Lwt.bind
             (Observe_lwt_unix.with_operation ~name:"parent" (fun () ->
                  Observe.Logs.info (text ~tag:"parent-before" "message");
                  Lwt.bind
                    (Observe_lwt_unix.fork ~name:"child" (fun () ->
                         Lwt.bind (Lwt.pause ()) (fun () ->
                             Observe.Logs.info
                               (text ~tag:"child-inside" "message");
                             Lwt.return_unit)))
                    (fun () ->
                      Observe.Logs.info (text ~tag:"parent-after" "message");
                      Lwt.return_unit)))
             (fun () ->
               Lwt.bind
                 (Lwt.both
                    (Observe_lwt_unix.with_operation ~name:"left" (fun () ->
                         Lwt.bind (Lwt.pause ()) (fun () ->
                             Observe.Logs.info (text ~tag:"left" "message");
                             Lwt.return_unit)))
                    (Observe_lwt_unix.with_operation ~name:"right" (fun () ->
                         Lwt.bind (Lwt.pause ()) (fun () ->
                             Observe.Logs.info (text ~tag:"right" "message");
                             Lwt.return_unit))))
                 (fun _ ->
                   Observe.Logs.info (text ~tag:"outside" "message");
                   Lwt.return capture))))
  in
  let parent_id = correlation_by_tag capture "parent-before" in
  let child_id = correlation_by_tag capture "child-inside" in
  check (Option.is_some parent_id) "parent scope did not correlate";
  check
    (Option.is_some child_id && child_id <> parent_id)
    "nested child scope did not override parent";
  check
    (correlation_by_tag capture "parent-after" = parent_id)
    "nested child did not restore parent";
  let left_id = correlation_by_tag capture "left" in
  let right_id = correlation_by_tag capture "right" in
  check (Option.is_some left_id) "left operation was not current";
  check (Option.is_some right_id) "right operation was not current";
  check (left_id <> right_id) "concurrent operation scopes leaked";
  check
    (correlation_by_tag capture "outside" = None)
    "scope installed a fallback operation";
  let wide_logs = Observe.Capture.logs capture |> List.filter is_wide in
  let named name =
    List.find
      (fun log ->
        String.equal (Observe.Log.operation_name (wide_operation log)) name)
      wide_logs
  in
  let child_operation = wide_operation (named "child") in
  check
    (Option.map Observe.Log.operation_reference_id
       (Observe.Log.operation_parent child_operation)
    = parent_id)
    "child lost its parent reference";
  check
    (Some (operation_id (named "parent")) = parent_id)
    "parent identity changed"

let operation_lifecycle () =
  Printexc.record_backtrace true;
  let escaped = Failure "operation-lwt" in
  let original =
    try failwith "operation-lwt-origin"
    with Failure _ -> Printexc.get_raw_backtrace ()
  in
  let caught = ref None in
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn
         ~config:(config "operation-lifecycle") (fun capture ->
           let marker = ref 9 in
           Lwt.bind
             (Observe_lwt_unix.with_operation ~name:"success" (fun () ->
                  Lwt.bind (Lwt.pause ()) (fun () -> Lwt.return marker)))
             (fun returned ->
               check (returned == marker) "operation Lwt result was replaced";
               Lwt.bind
                 (Lwt.catch
                    (fun () ->
                      Observe_lwt_unix.with_operation ~name:"failure" (fun () ->
                          Lwt.bind (Lwt.pause ()) (fun () ->
                              Printexc.raise_with_backtrace escaped original)))
                    (fun raised ->
                      caught := Some (raised, Printexc.get_raw_backtrace ());
                      Lwt.return_unit))
                 (fun () ->
                   let pending, _ = Lwt.task () in
                   let operation =
                     Observe_lwt_unix.with_operation ~name:"cancelled"
                       (fun () -> pending)
                   in
                   Lwt.cancel operation;
                   Lwt.bind
                     (Lwt.catch
                        (fun () -> operation)
                        (function
                          | Lwt.Canceled -> Lwt.return_unit
                          | raised -> Lwt.fail raised))
                     (fun () ->
                       Observe_lwt_unix.with_operation ~name:"parent" (fun () ->
                           Lwt.bind
                             (Observe_lwt_unix.fork ~name:"child" (fun () ->
                                  Observe.Logs.info
                                    (text ~tag:"fork-child" "message");
                                  Lwt.return 42))
                             (fun result ->
                               check (result = 42)
                                 "child operation result was replaced";
                               Observe.Logs.info
                                 (text ~tag:"fork-parent" "message");
                               Lwt.return capture)))))))
  in
  (match !caught with
  | Some (raised, backtrace) ->
      check (raised == escaped) "operation Lwt exception identity changed";
      let original = Printexc.raw_backtrace_to_string original in
      let propagated = Printexc.raw_backtrace_to_string backtrace in
      check
        (String.length propagated >= String.length original
        && String.sub propagated 0 (String.length original) = original)
        "operation Lwt backtrace origin changed"
  | None -> fail "operation Lwt failure was swallowed");
  let wide_logs = Observe.Capture.logs capture |> List.filter is_wide in
  check (List.length wide_logs = 5) "operation boundaries did not emit once";
  let by_name name =
    List.find
      (fun log ->
        String.equal (Observe.Log.operation_name (wide_operation log)) name)
      wide_logs
  in
  check
    (Observe.Level.equal Observe.Level.Error
       (Observe.Log.level (by_name "failure")))
    "ordinary failure did not derive Error";
  check
    (Observe.Level.equal Observe.Level.Info
       (Observe.Log.level (by_name "cancelled")))
    "cancellation inferred Error";
  check
    (correlation_by_tag capture "fork-child"
    = Some (operation_id (by_name "child")))
    "child operation was not current";
  check
    (correlation_by_tag capture "fork-parent"
    = Some (operation_id (by_name "parent")))
    "child operation did not restore parent"

let operation_late_callback () =
  let current_rejected = ref false in
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn ~config:(config "operation-late")
         (fun capture ->
           let trigger, wake_trigger = Lwt.wait () in
           let late = ref Lwt.return_unit in
           let pending, _ = Lwt.task () in
           let operation =
             Observe_lwt_unix.with_operation ~name:"cancelled" (fun () ->
                 late :=
                   Lwt.bind trigger (fun () ->
                       (current_rejected :=
                          match Observe.Logs.current () with
                          | _ -> false
                          | exception
                              Observe.Logs.Current_error Observe.Logs.Not_bound
                            ->
                              true);
                       Observe.Logs.info (text ~tag:"late-operation" "message");
                       Lwt.return_unit);
                 pending)
           in
           Lwt.cancel operation;
           Lwt.bind
             (Lwt.catch
                (fun () -> operation)
                (function
                  | Lwt.Canceled -> Lwt.return_unit | raised -> Lwt.fail raised))
             (fun () ->
               Lwt.wakeup wake_trigger ();
               Lwt.bind !late (fun () -> Lwt.return capture))))
  in
  check
    (correlation_by_tag capture "late-operation" = None)
    "late callback retained a closed operation scope";
  check !current_rejected "late callback retrieved a closed operation handle";
  check
    (List.length (List.filter is_wide (Observe.Capture.logs capture)) = 1)
    "cancelled operation did not complete exactly once"

let operation_foreign_execution () =
  let foreign_current_rejected = ref false in
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn
         ~config:(config "operation-foreign") (fun capture ->
           Lwt.bind
             (Observe_lwt_unix.with_operation ~name:"parent" (fun () ->
                  Lwt.bind
                    (Lwt_preemptive.detach
                       (fun () ->
                         (foreign_current_rejected :=
                            match Observe.Logs.current () with
                            | _ -> false
                            | exception
                                Observe.Logs.Current_error
                                  Observe.Logs.Not_bound ->
                                true);
                         Observe.Logs.info
                           (text ~tag:"foreign-operation" "message"))
                       ())
                    (fun () ->
                      Observe.Logs.info
                        (text ~tag:"restored-operation" "message");
                      Lwt.return_unit)))
             (fun () -> Lwt.return capture)))
  in
  check !foreign_current_rejected
    "foreign execution retrieved an inherited operation";
  check
    (correlation_by_tag capture "foreign-operation" = None)
    "foreign execution inherited operation correlation";
  check
    (match correlation_by_tag capture "restored-operation" with
    | Some id ->
        List.exists
          (fun log ->
            match Observe.Log.kind log with
            | Observe.Log.Wide { operation; _ } ->
                String.equal (Observe.Log.operation_id operation) id
            | Observe.Log.Point _ -> false)
          (Observe.Capture.logs capture)
    | None -> false)
    "foreign execution disturbed the parent operation"

let capture_then_init () =
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn ~config:(config "before-init")
         (fun capture ->
           Observe.Logs.info (text ~tag:"capture" "message");
           Lwt.return capture))
  in
  check (capture_tags capture = [ "capture" ]) "pre-init capture failed";
  let (), output =
    capture_stderr (fun () ->
        Observe_lwt_unix.init_exn (config "after-capture");
        Observe.Logs.info (text ~tag:"production" "message"))
  in
  check
    (contains output " INFO [production] message\n")
    "production did not initialize after capture: %S" output

let concurrent_capture () =
  let release, wake_release = Lwt.wait () in
  let entered = ref 0 in
  let run service tag =
    Observe_lwt_unix.Test.with_capture_exn ~config:(config service)
      (fun capture ->
        incr entered;
        if !entered = 2 then Lwt.wakeup wake_release ();
        Lwt.bind release (fun () ->
            Observe.Logs.info (text ~tag "message");
            Lwt.return capture))
  in
  let left, right =
    Lwt_main.run (Lwt.both (run "left" "left") (run "right" "right"))
  in
  check (capture_tags left = [ "left" ]) "left scope leaked";
  check (capture_tags right = [ "right" ]) "right scope leaked"

let nested_capture () =
  let outer, inner =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn ~config:(config "outer")
         (fun outer ->
           Observe.Logs.info (text ~tag:"outer-before" "message");
           Lwt.bind
             (Observe_lwt_unix.Test.with_capture_exn ~config:(config "inner")
                (fun inner ->
                  Observe.Logs.info (text ~tag:"inner" "message");
                  Lwt.return inner))
             (fun inner ->
               Observe.Logs.info (text ~tag:"outer-after" "message");
               Lwt.return (outer, inner))))
  in
  check
    (capture_tags outer = [ "outer-before"; "outer-after" ])
    "outer scope was not restored";
  check (capture_tags inner = [ "inner" ]) "inner scope was not isolated"

let exception_restoration () =
  let inner = ref None in
  let outer =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn ~config:(config "outer-error")
         (fun outer ->
           Lwt.catch
             (fun () ->
               Observe_lwt_unix.Test.with_capture_exn
                 ~config:(config "inner-error") (fun capture ->
                   inner := Some capture;
                   Observe.Logs.info (text ~tag:"inner-error" "message");
                   Lwt.fail Exit))
             (function
               | Exit ->
                   Observe.Logs.info (text ~tag:"outer-restored" "message");
                   Lwt.return outer
               | exn -> Lwt.fail exn)))
  in
  check
    (capture_tags outer = [ "outer-restored" ])
    "ordinary exception did not restore the outer scope";
  match !inner with
  | None -> fail "inner exception scope did not start"
  | Some capture ->
      check
        (capture_tags capture = [ "inner-error" ])
        "inner exception scope lost its admitted log"

let cancellation () =
  let capture = ref None in
  let trigger, wake_trigger = Lwt.wait () in
  let late = ref Lwt.return_unit in
  let pending, _ = Lwt.task () in
  let promise =
    Observe_lwt_unix.Test.with_capture_exn ~config:(config "cancel")
      (fun current ->
        capture := Some current;
        late :=
          Lwt.bind trigger (fun () ->
              Observe.Logs.info (text ~tag:"cancelled-late" "message");
              Lwt.return_unit);
        pending)
  in
  Lwt.cancel promise;
  (match Lwt.state promise with
  | Lwt.Fail Lwt.Canceled -> ()
  | Lwt.Return _ | Lwt.Sleep | Lwt.Fail _ ->
      fail "cancellation was not preserved");
  Lwt.wakeup wake_trigger ();
  Lwt_main.run !late;
  match !capture with
  | None -> fail "capture callback did not start"
  | Some capture ->
      check (Observe.Capture.logs capture = []) "cancelled scope retained a log";
      check
        (diagnostic_count capture Observe.Diagnostics.Capture_closed = 1)
        "cancellation did not close the capture before late work"

let late_callback () =
  let trigger, wake_trigger = Lwt.wait () in
  let late = ref Lwt.return_unit in
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn ~config:(config "late")
         (fun capture ->
           late :=
             Lwt.bind trigger (fun () ->
                 Observe.Logs.info (text ~tag:"late" "message");
                 Lwt.return_unit);
           Lwt.return capture))
  in
  Lwt.wakeup wake_trigger ();
  Lwt_main.run !late;
  check (Observe.Capture.logs capture = []) "late log reached closed capture";
  check
    (diagnostic_count capture Observe.Diagnostics.Capture_closed = 1)
    "late log did not diagnose closed capture"

let invalid_capacity () =
  let promise =
    Observe_lwt_unix.Test.with_capture_exn ~config:(config "invalid")
      ~capacity:0 (fun _ -> Lwt.return_unit)
  in
  match Lwt_main.run promise with
  | exception Observe_lwt_unix.Test.Capture_error (Observe.Invalid_capacity 0)
    ->
      ()
  | exception exn ->
      fail "unexpected capture exception: %s" (Printexc.to_string exn)
  | () -> fail "invalid capacity unexpectedly succeeded"

let scenarios =
  [
    ("clock", clock);
    ("default-identity", default_identity);
    ("custom-identity", custom_identity);
    ("failing-identity", failing_identity);
    ("custom-sampling", custom_sampling);
    ("unused-custom-sampling", unused_custom_sampling);
    ("stable-custom-sampling", stable_custom_sampling);
    ("reentrant-sampling", reentrant_sampling);
    ("console", console);
    ("json-console", json_console);
    ("wide-json-console", wide_json_console);
    ("production-json-console", production_json_console);
    ("repeated-init", repeated_init);
    ("init-rollback", init_rollback);
    ("silent-drain", silent_drain);
    ("no-output", no_output);
    ("bounded-console", bounded_console);
    ("serialized-console", serialized_console);
    ("shutdown", shutdown);
    ("shutdown-before-init", shutdown_before_init);
    ("prepared-shutdown", prepared_shutdown);
    ("active-capture-shutdown", active_capture_survives_shutdown);
    ("initialization-rebinds-owner", initialization_rebinds_owner);
    ("lifecycle-flush", lifecycle_flush);
    ("lifecycle-init-transfer", lifecycle_init_transfer);
    ("lifecycle-failure", lifecycle_failure);
    ("basic-capture", basic_capture);
    ("operation-scope", operation_scope);
    ("operation-lifecycle", operation_lifecycle);
    ("operation-late", operation_late_callback);
    ("operation-foreign", operation_foreign_execution);
    ("capture-then-init", capture_then_init);
    ("concurrent-capture", concurrent_capture);
    ("nested-capture", nested_capture);
    ("exception-restoration", exception_restoration);
    ("cancellation", cancellation);
    ("late-callback", late_callback);
    ("invalid-capacity", invalid_capacity);
  ]

let () =
  match Array.to_list Sys.argv with
  | [ _; name ] -> (
      match List.assoc_opt name scenarios with
      | Some scenario -> scenario ()
      | None -> fail "unknown scenario: %s" name)
  | _ -> fail "expected one scenario name"
