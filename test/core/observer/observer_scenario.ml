module Observer = Observe.Make (Test_io.IO)

let untyped make (builder : Observe.Logs.builder) = builder.untyped (make ())

let typed description make (builder : Observe.Logs.builder) =
  builder.typed description (make ())

module Raising_get_io = struct
  include Test_io.IO

  let get _ _ = failwith "scope lookup"
end

module Raising_get_observer = Observe.Make (Raising_get_io)
module Inherited_observer = Observe.Make (Test_io.Inherited_io)

let make_observer ?console_style ?now ?offer_console () =
  let host = Test_io.Host.create ?console_style ?now ?offer_console () in
  Observer.create host

let config ?enabled ?console ?min_level ?drains () =
  Test_io.config ?enabled ?console ?min_level ?drains "observer"

let inherited_config ?enabled ?console ?min_level ?drains () =
  Test_io.config ?enabled ?console ?min_level ?drains "inherited"

let init_ok observer config =
  match Observer.init observer config with
  | Ok () -> ()
  | Error Observe.Already_initialized ->
      Alcotest.fail "observer was already initialized"
  | Error Observe.IO_already_registered ->
      Alcotest.fail "a different I/O implementation already owned the route"

let init_inherited_ok observer config =
  match Inherited_observer.init observer config with
  | Ok () -> ()
  | Error Observe.Already_initialized ->
      Alcotest.fail "inherited observer was already initialized"
  | Error Observe.IO_already_registered ->
      Alcotest.fail "a different I/O implementation already owned the route"

let capture_tags capture =
  List.map
    (fun log ->
      match Test_io.text_payload log with
      | Some (tag, message) -> tag ^ ":" ^ message
      | None -> Alcotest.fail "expected a text body")
    (Observe.Capture.logs capture)

let check_capture_tags name expected capture =
  Alcotest.(check string)
    name
    (String.concat "," expected)
    (String.concat "," (capture_tags capture))

let not_initialized () =
  let forced = ref 0 in
  Observe.Logs.info
    (untyped (fun () ->
         incr forced;
         Observe.Value.int 1));
  Alcotest.(check int) "unrouted authoring is not evaluated" 0 !forced;
  Alcotest.(check int)
    "unrouted emission diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Not_initialized)

let inert_create () =
  ignore (make_observer ());
  let owner = make_observer () in
  init_ok owner (config ());
  Observe.Logs.info (Test_io.text ~tag:"owner" "installed")

let repeated_install () =
  let observer = make_observer () in
  init_ok observer (config ());
  Alcotest.(check bool)
    "second install is rejected" true
    (Observer.init observer (config ()) = Error Observe.Already_initialized);
  Alcotest.check_raises "init_exn preserves error"
    (Observe.Init_error Observe.Already_initialized) (fun () ->
      Observer.init_exn observer (config ()))

let io_owner_conflict () =
  let owner = make_observer () in
  let other = make_observer () in
  let capture_claim =
    Observer.with_capture owner (config ()) (fun _ -> Test_io.Direct.return ())
  in
  Alcotest.(check bool)
    "first I/O implementation claims through capture" true
    (capture_claim = Ok ());
  Alcotest.(check bool)
    "different I/O implementation cannot initialize" true
    (Observer.init other (config ()) = Error Observe.IO_already_registered)

let invalid_capacity () =
  let observer = make_observer () in
  let called = ref false in
  let result =
    Observer.with_capture observer (config ()) ~capacity:0 (fun _ ->
        called := true;
        Test_io.Direct.return ())
  in
  Alcotest.(check bool)
    "invalid capacity result" true
    (result = Error (Observe.Invalid_capacity 0));
  Alcotest.(check bool) "callback not entered" false !called;
  init_ok observer (config ())

let threshold_and_authoring () =
  let console = ref [] in
  let observer =
    make_observer
      ~offer_console:(fun output ->
        console := output :: !console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ~min_level:Observe.Level.Warn ());
  let forced = ref 0 in
  Observe.Logs.info (fun m ->
      incr forced;
      m.text ~tag:"threshold" "filtered");
  Observe.Logs.warn (fun m ->
      incr forced;
      m.text ~tag:"threshold" "accepted");
  Alcotest.(check int) "only admitted message authored" 1 !forced;
  Alcotest.(check int)
    "only admitted message delivered" 1 (List.length !console);
  Alcotest.(check char)
    "engine owns console newline" '\n'
    (List.hd !console).[String.length (List.hd !console) - 1]

let clock_unavailable () =
  let console = ref 0 in
  let observer =
    make_observer
      ~now:(fun () -> Error Observe.IO.Unavailable)
      ~offer_console:(fun _ ->
        incr console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ());
  Observe.Logs.info (Test_io.text ~tag:"clock" "missing");
  Alcotest.(check int) "no console delivery" 0 !console;
  Alcotest.(check int)
    "clock diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Clock_unavailable)

let scope_raised () =
  let observer = Raising_get_observer.create (Test_io.Host.create ()) in
  (match
     Raising_get_observer.init observer
       (config ~console:Observe.Config.Silent ())
   with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "raising I/O implementation failed to initialize");
  Observe.Logs.info (Test_io.text ~tag:"scope" "lookup");
  Alcotest.(check int)
    "scope lookup exception diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Capture_lookup_raised)

let formatting_failed () =
  let console = ref 0 in
  let observer =
    make_observer
      ~offer_console:(fun _ ->
        incr console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ());
  Observe.Logs.info (Test_io.text ~tag:"format" (String.make 1 (Char.chr 0xff)));
  Alcotest.(check int) "invalid output not delivered" 0 !console;
  Alcotest.(check int)
    "formatting failure diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Formatting_failed);
  let raising_json =
    ((fun _ _ -> raise Exit), fun _ -> Error (`Msg "unused decoder"))
  in
  let raising_description =
    Observe.Type.of_repr (Repr.like ~json:raising_json Repr.int)
  in
  Observe.Logs.info (typed raising_description (fun () -> 1));
  Alcotest.(check int) "raised output not delivered" 0 !console;
  Alcotest.(check int)
    "Repr callback exception diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Formatting_raised)

let callback_containment () =
  let clock_calls = ref 0 in
  let observer =
    make_observer
      ~now:(fun () ->
        incr clock_calls;
        if !clock_calls = 1 then Ok (Observe.Timestamp.of_unix_ns 1L)
        else failwith "clock")
      ()
  in
  init_ok observer
    (config ~console:Observe.Config.Silent
       ~drains:[ Observe.Drain.create (fun _ -> raise Exit) ]
       ());
  Observe.Logs.info (untyped (fun () -> failwith "authoring"));
  Observe.Logs.info (Test_io.text ~tag:"drain" "raises");
  Alcotest.(check int)
    "authoring diagnosed" 1
    (Test_io.process_diagnostic_count
       Observe.Diagnostics.Message_evaluation_raised);
  Alcotest.(check int)
    "raising clock diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Clock_raised)

let console_and_drains () =
  let accepted = ref 0 in
  let rejected = ref 0 in
  let raised = ref 0 in
  let drains =
    [
      Observe.Drain.create (fun _ ->
          incr accepted;
          Observe.Drain.Accepted);
      Observe.Drain.create (fun _ ->
          incr rejected;
          Observe.Drain.Rejected);
      Observe.Drain.create (fun _ ->
          incr raised;
          failwith "drain");
    ]
  in
  let observer =
    make_observer ~offer_console:(fun _ -> Observe.IO.Rejected) ()
  in
  init_ok observer (config ~drains ());
  Observe.Logs.info (Test_io.text ~tag:"fanout" "message");
  Alcotest.(check int) "accepted drain called" 1 !accepted;
  Alcotest.(check int) "rejected drain called" 1 !rejected;
  Alcotest.(check int) "raising drain called" 1 !raised;
  Alcotest.(check int)
    "console rejection diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Console_rejected);
  Alcotest.(check int)
    "drain rejection diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Drain_rejected);
  Alcotest.(check int)
    "drain exception diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Drain_raised)

let console_raised () =
  let observer =
    make_observer ~offer_console:(fun _ -> failwith "console") ()
  in
  init_ok observer (config ());
  Observe.Logs.info (Test_io.text ~tag:"console" "raises");
  Alcotest.(check int)
    "console exception diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Console_raised)

let ansi_console () =
  let output = Buffer.create 128 in
  let observer =
    make_observer ~console_style:Observe.Formatter.Ansi_16
      ~now:(fun () -> Ok (Observe.Timestamp.of_unix_ns 37_425_612_000_000L))
      ~offer_console:(fun value ->
        Buffer.add_string output value;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ());
  Observe.Logs.info (Test_io.text ~tag:"tag" "message");
  Alcotest.(check string)
    "engine selects ANSI formatter"
    "\027[90m10:23:45.612\027[0m \027[1;96mINFO\027[0m \027[1;96m[tag]\027[0m \
     message\n"
    (Buffer.contents output)

let disabled () =
  let console = ref 0 in
  let observer =
    make_observer
      ~offer_console:(fun _ ->
        incr console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ~enabled:false ());
  let forced = ref 0 in
  Observe.Logs.info
    (untyped (fun () ->
         incr forced;
         Observe.Value.int 1));
  Alcotest.(check int) "disabled logging is not authored" 0 !forced;
  Alcotest.(check int) "disabled logging has no output" 0 !console;
  Alcotest.(check int)
    "disabled configuration is not diagnosed as outputless" 0
    (Test_io.process_diagnostic_count Observe.Diagnostics.No_delivery_target)

let no_output () =
  let observer = make_observer () in
  init_ok observer (config ~console:Observe.Config.Silent ());
  Alcotest.(check int)
    "enabled installation with no output is diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.No_delivery_target)

let scope_overrides_production () =
  let console = ref 0 in
  let observer =
    make_observer
      ~offer_console:(fun _ ->
        incr console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ());
  let capture =
    match
      Observer.with_capture observer (config ()) (fun capture ->
          Observe.Logs.info (Test_io.text ~tag:"scope" "captured");
          Test_io.Direct.return capture)
    with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "capture was rejected"
  in
  Alcotest.(check int) "scope suppresses console" 0 !console;
  Alcotest.(check int)
    "scope receives message" 1
    (List.length (Observe.Capture.logs capture));
  Observe.Logs.info (Test_io.text ~tag:"production" "restored");
  Alcotest.(check int) "production restored" 1 !console

let nested_capture_precedence () =
  let console = ref 0 in
  let observer =
    make_observer
      ~offer_console:(fun _ ->
        incr console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ());
  let outer, inner =
    match
      Observer.with_capture observer (config ()) (fun outer ->
          Observe.Logs.info (Test_io.text ~tag:"outer" "before-inner");
          let inner =
            match
              Observer.with_capture observer (config ()) (fun inner ->
                  Observe.Logs.info (Test_io.text ~tag:"inner" "inside");
                  Test_io.Direct.return inner)
            with
            | Ok inner -> inner
            | Error _ -> Alcotest.fail "nested capture was rejected"
          in
          Observe.Logs.info (Test_io.text ~tag:"outer" "after-inner");
          Test_io.Direct.return (outer, inner))
    with
    | Ok captures -> captures
    | Error _ -> Alcotest.fail "outer capture was rejected"
  in
  check_capture_tags "outer capture keeps only outer messages"
    [ "outer:before-inner"; "outer:after-inner" ]
    outer;
  check_capture_tags "inner capture takes precedence" [ "inner:inside" ] inner;
  Alcotest.(check int) "nested captures suppress production" 0 !console;
  Observe.Logs.info (Test_io.text ~tag:"production" "after-captures");
  Alcotest.(check int) "production restored after nested captures" 1 !console

let cancellation_capture_close () =
  let context = Test_io.Inherited.create_context () in
  let console = ref 0 in
  let observer =
    Inherited_observer.create
      (Test_io.Inherited_io.create ~context
         ~host:
           (Test_io.Host.create
              ~offer_console:(fun _ ->
                incr console;
                Observe.IO.Accepted)
              ()))
  in
  let capture = ref None in
  let inherited = ref None in
  let cancelled =
    try
      ignore
        (Inherited_observer.with_capture observer (inherited_config ())
           (fun current_capture ->
             capture := Some current_capture;
             inherited := Some (Test_io.Inherited.inherited_context context);
             Observe.Logs.info (Test_io.text ~tag:"cancel" "before-cancel");
             raise Test_io.Inherited.Cancelled));
      false
    with Test_io.Inherited.Cancelled -> true
  in
  Alcotest.(check bool) "native cancellation is preserved" true cancelled;
  Alcotest.(check int)
    "capture protect finalizer runs once" 1
    (Test_io.Inherited.finally_calls context);
  let capture =
    match !capture with
    | Some capture -> capture
    | None -> Alcotest.fail "cancellation did not enter capture"
  in
  check_capture_tags "cancelled capture retains admitted work"
    [ "cancel:before-cancel" ] capture;
  Observe.Logs.info (Test_io.text ~tag:"restored" "before-production-install");
  Alcotest.(check int)
    "binding restored before production install" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Not_initialized);
  init_inherited_ok observer (inherited_config ());
  let inherited =
    match !inherited with
    | Some inherited -> inherited
    | None -> Alcotest.fail "inherited context was not retained"
  in
  Test_io.Inherited.with_context context inherited (fun () ->
      Observe.Logs.info (Test_io.text ~tag:"detached" "after-cancel");
      Observe.Logs.info (Test_io.text ~tag:"detached" "after-cancel-again"));
  Alcotest.(check int)
    "closed cancelled capture withholds detached work" 0 !console;
  Alcotest.(check int)
    "each detached offer diagnoses the closed capture" 2
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Capture_closed);
  Observe.Logs.info (Test_io.text ~tag:"production" "restored");
  Alcotest.(check int) "production restored after cancellation" 1 !console

let closed_inherited_tombstone () =
  let context = Test_io.Inherited.create_context () in
  let console = ref 0 in
  let observer =
    Inherited_observer.create
      (Test_io.Inherited_io.create ~context
         ~host:
           (Test_io.Host.create
              ~offer_console:(fun _ ->
                incr console;
                Observe.IO.Accepted)
              ()))
  in
  let inherited = ref None in
  let capture =
    match
      Inherited_observer.with_capture observer (inherited_config ())
        (fun current_capture ->
          inherited := Some (Test_io.Inherited.inherited_context context);
          Observe.Logs.info (Test_io.text ~tag:"capture" "before-detach");
          Test_io.Inherited.return current_capture)
    with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "capture was rejected"
  in
  init_inherited_ok observer (inherited_config ());
  let inherited =
    match !inherited with
    | Some inherited -> inherited
    | None -> Alcotest.fail "inherited context was not retained"
  in
  Test_io.Inherited.with_context context inherited (fun () ->
      Observe.Logs.info (Test_io.text ~tag:"detached" "closed-scope"));
  Alcotest.(check int)
    "closed inherited scope withholds from production" 0 !console;
  Alcotest.(check int)
    "closed inherited scope is diagnosed" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Capture_closed);
  Observe.Logs.info (Test_io.text ~tag:"production" "after-detach");
  Alcotest.(check int) "root binding restored after detached work" 1 !console

let callback_restoration () =
  let console = ref 0 in
  let observer =
    make_observer
      ~offer_console:(fun _ ->
        incr console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ());
  Alcotest.check_raises "callback exception preserved" (Failure "callback")
    (fun () ->
      ignore
        (Observer.with_capture observer (config ()) (fun _ ->
             raise (Failure "callback"))));
  Observe.Logs.info (Test_io.text ~tag:"after" "production");
  Alcotest.(check int) "binding restored after exception" 1 !console

let control_exception () =
  let observer = make_observer () in
  init_ok observer
    (config ~console:Observe.Config.Silent
       ~drains:[ Observe.Drain.create (fun _ -> Observe.Drain.Accepted) ]
       ());
  Alcotest.check_raises "I/O control exception preserved" Test_io.Direct.Control
    (fun () ->
      Observe.Logs.info (untyped (fun () -> raise Test_io.Direct.Control)))

let control_backtrace () =
  Printexc.record_backtrace true;
  let original =
    try failwith "raw-backtrace-sentinel"
    with Failure _ -> Printexc.get_raw_backtrace ()
  in
  let original_text = Printexc.raw_backtrace_to_string original in
  Alcotest.(check bool)
    "raw backtrace recording is available" true
    (String.length original_text > 0);
  let observer = make_observer () in
  let actual =
    try
      ignore
        (Observer.with_capture observer
           (config ~console:Observe.Config.Silent ()) (fun _ ->
             Observe.Logs.info
               (untyped (fun () ->
                    Printexc.raise_with_backtrace Test_io.Direct.Control
                      original));
             Test_io.Direct.return ()));
      None
    with Test_io.Direct.Control -> Some (Printexc.get_raw_backtrace ())
  in
  match actual with
  | None -> Alcotest.fail "protected control exception was contained"
  | Some actual ->
      let actual = Printexc.raw_backtrace_to_string actual in
      let preserves_origin =
        String.length actual >= String.length original_text
        && String.sub actual 0 (String.length original_text) = original_text
      in
      Alcotest.(check bool)
        "protected control raw backtrace is preserved" true preserves_origin

let scenario = function
  | "not-initialized" -> not_initialized
  | "inert-create" -> inert_create
  | "repeated-install" -> repeated_install
  | "io-owner-conflict" -> io_owner_conflict
  | "invalid-capacity" -> invalid_capacity
  | "threshold-and-authoring" -> threshold_and_authoring
  | "clock-unavailable" -> clock_unavailable
  | "scope-raised" -> scope_raised
  | "formatting-failed" -> formatting_failed
  | "callback-containment" -> callback_containment
  | "console-and-drains" -> console_and_drains
  | "ANSI-console" -> ansi_console
  | "console-raised" -> console_raised
  | "disabled" -> disabled
  | "no-output" -> no_output
  | "scope-overrides-production" -> scope_overrides_production
  | "nested-capture" -> nested_capture_precedence
  | "cancellation-capture" -> cancellation_capture_close
  | "closed-inherited-tombstone" -> closed_inherited_tombstone
  | "callback-restoration" -> callback_restoration
  | "control-exception" -> control_exception
  | "control-backtrace" -> control_backtrace
  | name -> fun () -> Alcotest.failf "unknown observer scenario: %s" name

let () =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "missing" in
  (scenario name) ()
