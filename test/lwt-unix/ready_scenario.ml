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

let config ?environment ?console ?drains service =
  Observe.Config.create_exn ~service ?environment ?console ?drains ()

let text_tag log =
  match Observe.Log.body log with
  | Observe.Log.Text { tag; _ } -> tag
  | Observe.Log.Structured _ -> fail "expected text log"

let capture_tags capture = List.map text_tag (Observe.Capture.logs capture)

let diagnostic_count capture kind =
  List.fold_left
    (fun total (entry : Observe.Diagnostics.entry) ->
      if entry.kind = kind then total + entry.count else total)
    0
    (Observe.Capture.diagnostics capture)

let operation_id log =
  match Observe.Log.operation log with
  | Some operation -> Observe.Log.operation_id operation
  | None -> fail "expected a wide operation"

let correlation_by_tag capture tag =
  Observe.Capture.logs capture
  |> List.find_map (fun log ->
      match Observe.Log.body log with
      | Observe.Log.Text { tag = actual; _ } when String.equal actual tag ->
          Some (Observe.Log.correlation_id log)
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
    (contains output "\"operation\":{\"name\":\"checkout\",\"id\":")
    "wide operation envelope missing from ready JSON: %S" output;
  check
    (contains output "\"duration_ns\":\"")
    "wide duration missing from ready JSON: %S" output;
  check
    (contains output "\"body\":{\"cart_id\":\"cart-1\"}")
    "wide body missing from ready JSON: %S" output;
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
  let (), output =
    capture_stderr (fun () ->
        Observe_lwt_unix.init_exn (config "shutdown");
        Observe.Logs.info (text ~tag:"shutdown" "before");
        Lwt_main.run (Observe_lwt_unix.shutdown ());
        Observe.Logs.info (text ~tag:"shutdown" "after");
        check
          (process_diagnostic_count Observe.Diagnostics.Console_rejected = 1)
          "post-shutdown output was not rejected")
  in
  check (contains output "before") "shutdown lost an accepted record";
  check (not (contains output "after")) "shutdown accepted a later record";
  Lwt_main.run (Observe_lwt_unix.shutdown ())

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
  check (!attempted = 2) "failure prevented another shutdown hook"

let basic_capture () =
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn (config "capture") (fun capture ->
           Observe.Logs.info (text ~tag:"captured" "message");
           Lwt.return capture))
  in
  check (capture_tags capture = [ "captured" ]) "capture missed the log"

let wide_scope () =
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn (config "wide-scope")
         (fun capture ->
           let parent = Observe.Logs.create ~name:"parent" () in
           let child = Observe.Logs.create ~parent ~name:"child" () in
           let left = Observe.Logs.create ~name:"left" () in
           let right = Observe.Logs.create ~name:"right" () in
           Lwt.bind
             (Observe_lwt_unix.with_wide parent (fun () ->
                  Observe.Logs.info (text ~tag:"parent-before" "message");
                  Lwt.bind
                    (Observe_lwt_unix.with_wide child (fun () ->
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
                    (Observe_lwt_unix.with_wide left (fun () ->
                         Lwt.bind (Lwt.pause ()) (fun () ->
                             Observe.Logs.info (text ~tag:"left" "message");
                             Lwt.return_unit)))
                    (Observe_lwt_unix.with_wide right (fun () ->
                         Lwt.bind (Lwt.pause ()) (fun () ->
                             Observe.Logs.info (text ~tag:"right" "message");
                             Lwt.return_unit))))
                 (fun _ ->
                   Observe.Logs.info (text ~tag:"outside" "message");
                   List.iter Observe.Logs.emit [ child; parent; left; right ];
                   Lwt.return capture))))
  in
  check
    (correlation_by_tag capture "parent-before" = Some "operation-1")
    "parent scope did not correlate";
  check
    (correlation_by_tag capture "child-inside" = Some "operation-2")
    "nested child scope did not override parent";
  check
    (correlation_by_tag capture "parent-after" = Some "operation-1")
    "nested child did not restore parent";
  check
    (correlation_by_tag capture "left" = Some "operation-3")
    "left concurrent scope leaked";
  check
    (correlation_by_tag capture "right" = Some "operation-4")
    "right concurrent scope leaked";
  check
    (correlation_by_tag capture "outside" = None)
    "scope installed a fallback operation";
  let wide_logs =
    Observe.Capture.logs capture
    |> List.filter (fun log -> Observe.Log.kind log = Observe.Log.Wide)
  in
  match wide_logs with
  | child :: parent :: _ ->
      let child_operation = Option.get (Observe.Log.operation child) in
      check
        (Observe.Log.operation_parent_id child_operation = Some "operation-1")
        "child lost its parent reference";
      check (operation_id parent = "operation-1") "parent identity changed"
  | _ -> fail "expected completed wide logs"

let managed_wide () =
  Printexc.record_backtrace true;
  let escaped = Failure "managed-lwt" in
  let original =
    try failwith "managed-lwt-origin"
    with Failure _ -> Printexc.get_raw_backtrace ()
  in
  let caught = ref None in
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn (config "managed-wide")
         (fun capture ->
           let success = Observe.Logs.create ~name:"success" () in
           let marker = ref 9 in
           Lwt.bind
             (Observe_lwt_unix.manage success ~error:Observe.Error.exn
                (fun () ->
                  Lwt.bind (Lwt.pause ()) (fun () -> Lwt.return marker)))
             (fun returned ->
               check (returned == marker) "managed Lwt result was replaced";
               let failure = Observe.Logs.create ~name:"failure" () in
               Lwt.bind
                 (Lwt.catch
                    (fun () ->
                      Observe_lwt_unix.manage failure ~error:Observe.Error.exn
                        (fun () ->
                          Lwt.bind (Lwt.pause ()) (fun () ->
                              Printexc.raise_with_backtrace escaped original)))
                    (fun raised ->
                      caught := Some (raised, Printexc.get_raw_backtrace ());
                      Lwt.return_unit))
                 (fun () ->
                   let cancelled = Observe.Logs.create ~name:"cancelled" () in
                   let pending, _ = Lwt.task () in
                   let managed =
                     Observe_lwt_unix.manage cancelled ~error:Observe.Error.exn
                       (fun () -> pending)
                   in
                   Lwt.cancel managed;
                   Lwt.bind
                     (Lwt.catch
                        (fun () -> managed)
                        (function
                          | Lwt.Canceled -> Lwt.return_unit
                          | raised -> Lwt.fail raised))
                     (fun () ->
                       let parent = Observe.Logs.create ~name:"parent" () in
                       Lwt.bind
                         (Observe_lwt_unix.with_wide parent (fun () ->
                              Lwt.bind
                                (Observe_lwt_unix.fork ~parent ~name:"child"
                                   ~error:Observe.Error.exn (fun _child ->
                                     Observe.Logs.info
                                       (text ~tag:"fork-child" "message");
                                     Lwt.return 42))
                                (fun result ->
                                  check (result = 42)
                                    "managed child result was replaced";
                                  Observe.Logs.info
                                    (text ~tag:"fork-parent" "message");
                                  Lwt.return_unit)))
                         (fun () ->
                           Observe.Logs.emit parent;
                           Lwt.return capture))))))
  in
  (match !caught with
  | Some (raised, backtrace) ->
      check (raised == escaped) "managed Lwt exception identity changed";
      let original = Printexc.raw_backtrace_to_string original in
      let propagated = Printexc.raw_backtrace_to_string backtrace in
      check
        (String.length propagated >= String.length original
        && String.sub propagated 0 (String.length original) = original)
        "managed Lwt backtrace origin changed"
  | None -> fail "managed Lwt failure was swallowed");
  let wide_logs =
    Observe.Capture.logs capture
    |> List.filter (fun log -> Observe.Log.kind log = Observe.Log.Wide)
  in
  check (List.length wide_logs = 5) "managed boundaries did not emit once";
  let by_name name =
    List.find
      (fun log ->
        match Observe.Log.operation log with
        | Some operation ->
            String.equal (Observe.Log.operation_name operation) name
        | None -> false)
      wide_logs
  in
  check
    (Observe.Level.equal Observe.Level.Error
       (Observe.Log.level (by_name "failure")))
    "managed ordinary failure did not derive Error";
  check
    (Observe.Level.equal Observe.Level.Info
       (Observe.Log.level (by_name "cancelled")))
    "managed cancellation inferred Error";
  check
    (correlation_by_tag capture "fork-child" = Some "operation-5")
    "managed child was not current";
  check
    (correlation_by_tag capture "fork-parent" = Some "operation-4")
    "managed child did not restore parent"

let managed_late_callback () =
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn (config "managed-late")
         (fun capture ->
           let wide = Observe.Logs.create ~name:"cancelled" () in
           let trigger, wake_trigger = Lwt.wait () in
           let late = ref Lwt.return_unit in
           let pending, _ = Lwt.task () in
           let managed =
             Observe_lwt_unix.manage wide ~error:Observe.Error.exn (fun () ->
                 late :=
                   Lwt.bind trigger (fun () ->
                       Observe.Logs.info (text ~tag:"late-operation" "message");
                       Lwt.return_unit);
                 pending)
           in
           Lwt.cancel managed;
           Lwt.bind
             (Lwt.catch
                (fun () -> managed)
                (function
                  | Lwt.Canceled -> Lwt.return_unit | raised -> Lwt.fail raised))
             (fun () ->
               Lwt.wakeup wake_trigger ();
               Lwt.bind !late (fun () -> Lwt.return capture))))
  in
  check
    (correlation_by_tag capture "late-operation" = None)
    "late callback retained a closed operation scope";
  check
    (List.length
       (List.filter
          (fun log -> Observe.Log.kind log = Observe.Log.Wide)
          (Observe.Capture.logs capture))
    = 1)
    "cancelled managed wide did not complete exactly once"

let capture_then_init () =
  let capture =
    Lwt_main.run
      (Observe_lwt_unix.Test.with_capture_exn (config "before-init")
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
    Observe_lwt_unix.Test.with_capture_exn (config service) (fun capture ->
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
      (Observe_lwt_unix.Test.with_capture_exn (config "outer") (fun outer ->
           Observe.Logs.info (text ~tag:"outer-before" "message");
           Lwt.bind
             (Observe_lwt_unix.Test.with_capture_exn (config "inner")
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
      (Observe_lwt_unix.Test.with_capture_exn (config "outer-error")
         (fun outer ->
           Lwt.catch
             (fun () ->
               Observe_lwt_unix.Test.with_capture_exn (config "inner-error")
                 (fun capture ->
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
    Observe_lwt_unix.Test.with_capture_exn (config "cancel") (fun current ->
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
      (Observe_lwt_unix.Test.with_capture_exn (config "late") (fun capture ->
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
    Observe_lwt_unix.Test.with_capture_exn (config "invalid") ~capacity:0
      (fun _ -> Lwt.return_unit)
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
    ("console", console);
    ("json-console", json_console);
    ("wide-json-console", wide_json_console);
    ("production-json-console", production_json_console);
    ("repeated-init", repeated_init);
    ("silent-drain", silent_drain);
    ("no-output", no_output);
    ("bounded-console", bounded_console);
    ("serialized-console", serialized_console);
    ("shutdown", shutdown);
    ("lifecycle-flush", lifecycle_flush);
    ("lifecycle-failure", lifecycle_failure);
    ("basic-capture", basic_capture);
    ("wide-scope", wide_scope);
    ("managed-wide", managed_wide);
    ("managed-late", managed_late_callback);
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
