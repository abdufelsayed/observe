module System =
  Observe.Runtime.Make (Test_runtime.Runtime) (Test_runtime.Platform)

module Raising_get_runtime = struct
  include Test_runtime.Runtime

  let get () _ = failwith "scope lookup"
end

module Raising_get_system =
  Observe.Runtime.Make (Raising_get_runtime) (Test_runtime.Platform)

module Inherited_system =
  Observe.Runtime.Make (Test_runtime.Inherited_runtime) (Test_runtime.Platform)

let make_system ?console_style ?now ?write_console () =
  let platform =
    Test_runtime.Platform.create ?console_style ?now ?write_console ()
  in
  System.create ~runtime_context:() ~platform

let config ?enabled ?pretty ?silent ?min_level ?drains () =
  Test_runtime.config ?enabled ?pretty ?silent ?min_level ?drains "runtime"

let inherited_config ?enabled ?pretty ?silent ?min_level ?drains () =
  Test_runtime.config ?enabled ?pretty ?silent ?min_level ?drains "inherited"

let init_ok system config =
  match System.init system config with
  | Ok () -> ()
  | Error Observe.Runtime.Already_initialized ->
      Alcotest.fail "runtime was already initialized"
  | Error Observe.Runtime.Runtime_already_registered ->
      Alcotest.fail "a different runtime already owned the route"

let init_inherited_ok system config =
  match Inherited_system.init system config with
  | Ok () -> ()
  | Error Observe.Runtime.Already_initialized ->
      Alcotest.fail "inherited runtime was already initialized"
  | Error Observe.Runtime.Runtime_already_registered ->
      Alcotest.fail "a different runtime already owned the route"

let capture_tags capture =
  List.map
    (fun log ->
      match Test_runtime.text_payload log with
      | Some (tag, message) -> tag ^ ":" ^ message
      | None -> Alcotest.fail "expected a text payload")
    (Observe.Capture.logs capture)

let check_capture_tags name expected capture =
  Alcotest.(check string)
    name
    (String.concat "," expected)
    (String.concat "," (capture_tags capture))

let not_initialized () =
  let forced = ref 0 in
  Observe.Logs.info
    (Observe.Logs.free (fun () ->
         incr forced;
         Observe.Value.int 1));
  Alcotest.(check int) "unrouted authoring remains lazy" 0 !forced;
  Alcotest.(check int)
    "unrouted emission diagnosed" 1
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.Not_initialized)

let inert_create () =
  ignore (make_system ());
  let owner = make_system () in
  init_ok owner (config ());
  Observe.Logs.info (Observe.Logs.text ~tag:"owner" "installed")

let repeated_install () =
  let system = make_system () in
  init_ok system (config ());
  Alcotest.(check bool)
    "second install is rejected" true
    (System.init system (config ()) = Error Observe.Runtime.Already_initialized);
  Alcotest.check_raises "init_exn preserves error"
    (Observe.Runtime.Init_error Observe.Runtime.Already_initialized) (fun () ->
      System.init_exn system (config ()))

let runtime_owner_conflict () =
  let owner = make_system () in
  let other = make_system () in
  let capture_claim =
    System.with_capture owner (config ()) (fun _ ->
        Test_runtime.Runtime.return ())
  in
  Alcotest.(check bool)
    "first runtime claims through capture" true (capture_claim = Ok ());
  Alcotest.(check bool)
    "different runtime cannot initialize" true
    (System.init other (config ())
    = Error Observe.Runtime.Runtime_already_registered)

let invalid_capacity () =
  let system = make_system () in
  let called = ref false in
  let result =
    System.with_capture system (config ()) ~capacity:0 (fun _ ->
        called := true;
        Test_runtime.Runtime.return ())
  in
  Alcotest.(check bool)
    "invalid capacity result" true
    (result = Error (Observe.Runtime.Invalid_capacity 0));
  Alcotest.(check bool) "callback not entered" false !called;
  init_ok system (config ())

let threshold_and_laziness () =
  let console = ref [] in
  let system =
    make_system
      ~write_console:(fun output ->
        console := output :: !console;
        Observe.Platform.Accepted)
      ()
  in
  init_ok system (config ~min_level:Observe.Level.Warn ());
  let forced = ref 0 in
  Observe.Logs.info
    (Observe.Logs.text_lazy ~tag:"threshold" (fun () ->
         incr forced;
         "filtered"));
  Observe.Logs.warn
    (Observe.Logs.text_lazy ~tag:"threshold" (fun () ->
         incr forced;
         "accepted"));
  Alcotest.(check int) "only admitted message forced" 1 !forced;
  Alcotest.(check int)
    "only admitted message delivered" 1 (List.length !console);
  Alcotest.(check char)
    "engine owns console newline" '\n'
    (List.hd !console).[String.length (List.hd !console) - 1]

let clock_unavailable () =
  let console = ref 0 in
  let system =
    make_system
      ~now:(fun () -> Error Observe.Platform.Unavailable)
      ~write_console:(fun _ ->
        incr console;
        Observe.Platform.Accepted)
      ()
  in
  init_ok system (config ());
  Observe.Logs.info (Observe.Logs.text ~tag:"clock" "missing");
  Alcotest.(check int) "no console delivery" 0 !console;
  Alcotest.(check int)
    "clock diagnosed" 1
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.Clock_unavailable)

let scope_raised () =
  let system =
    Raising_get_system.create ~runtime_context:()
      ~platform:(Test_runtime.Platform.create ())
  in
  (match Raising_get_system.init system (config ~silent:true ()) with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "raising runtime failed to initialize");
  Observe.Logs.info (Observe.Logs.text ~tag:"scope" "lookup");
  Alcotest.(check int)
    "scope lookup exception diagnosed" 1
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.Scope_raised)

let formatting_failed () =
  let console = ref 0 in
  let system =
    make_system
      ~write_console:(fun _ ->
        incr console;
        Observe.Platform.Accepted)
      ()
  in
  init_ok system (config ());
  Observe.Logs.info
    (Observe.Logs.text ~tag:"format" (String.make 1 (Char.chr 0xff)));
  Alcotest.(check int) "invalid output not delivered" 0 !console;
  Alcotest.(check int)
    "formatting failure diagnosed" 1
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.Formatting_failed);
  let raising_json =
    ((fun _ _ -> raise Exit), fun _ -> Error (`Msg "unused decoder"))
  in
  let raising_description =
    Observe.Type.of_repr (Repr.like ~json:raising_json Repr.int)
  in
  Observe.Logs.info (Observe.Logs.structured raising_description 1);
  Alcotest.(check int) "raised output not delivered" 0 !console;
  Alcotest.(check int)
    "Repr callback exception diagnosed" 1
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.Formatting_raised)

let callback_containment () =
  let clock_calls = ref 0 in
  let system =
    make_system
      ~now:(fun () ->
        incr clock_calls;
        if !clock_calls = 1 then Ok (Observe.Instant.of_epoch_nanoseconds 1L)
        else failwith "clock")
      ()
  in
  init_ok system
    (config ~silent:true
       ~drains:[ Observe.Drain.create (fun _ -> raise Exit) ]
       ());
  Observe.Logs.info (Observe.Logs.free (fun () -> failwith "authoring"));
  Observe.Logs.info (Observe.Logs.text ~tag:"drain" "raises");
  Alcotest.(check int)
    "authoring diagnosed" 1
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.Authoring_raised);
  Alcotest.(check int)
    "raising clock diagnosed" 1
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.Clock_raised)

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
  let system =
    make_system ~write_console:(fun _ -> Observe.Platform.Rejected) ()
  in
  init_ok system (config ~drains ());
  Observe.Logs.info (Observe.Logs.text ~tag:"fanout" "message");
  Alcotest.(check int) "accepted drain called" 1 !accepted;
  Alcotest.(check int) "rejected drain called" 1 !rejected;
  Alcotest.(check int) "raising drain called" 1 !raised;
  Alcotest.(check int)
    "console rejection diagnosed" 1
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.Console_rejected);
  Alcotest.(check int)
    "drain rejection diagnosed" 1
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.Drain_rejected);
  Alcotest.(check int)
    "drain exception diagnosed" 1
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.Drain_raised)

let console_raised () =
  let system = make_system ~write_console:(fun _ -> failwith "console") () in
  init_ok system (config ());
  Observe.Logs.info (Observe.Logs.text ~tag:"console" "raises");
  Alcotest.(check int)
    "console exception diagnosed" 1
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.Console_raised)

let ansi_console () =
  let output = Buffer.create 128 in
  let system =
    make_system ~console_style:Observe.Formatter.Ansi_16
      ~now:(fun () ->
        Ok (Observe.Instant.of_epoch_nanoseconds 37_425_612_000_000L))
      ~write_console:(fun value ->
        Buffer.add_string output value;
        Observe.Platform.Accepted)
      ()
  in
  init_ok system (config ());
  Observe.Logs.info (Observe.Logs.text ~tag:"tag" "message");
  Alcotest.(check string)
    "engine selects ANSI formatter"
    "\027[90m10:23:45.612\027[0m \027[1;96mINFO\027[0m \027[1;96m[tag]\027[0m \
     message\n"
    (Buffer.contents output)

let disabled () =
  let console = ref 0 in
  let system =
    make_system
      ~write_console:(fun _ ->
        incr console;
        Observe.Platform.Accepted)
      ()
  in
  init_ok system (config ~enabled:false ());
  let forced = ref 0 in
  Observe.Logs.info
    (Observe.Logs.free (fun () ->
         incr forced;
         Observe.Value.int 1));
  Alcotest.(check int) "disabled logging remains lazy" 0 !forced;
  Alcotest.(check int) "disabled logging has no output" 0 !console;
  Alcotest.(check int)
    "disabled configuration is not diagnosed as outputless" 0
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.No_output)

let no_output () =
  let system = make_system () in
  init_ok system (config ~silent:true ());
  Alcotest.(check int)
    "enabled installation with no output is diagnosed" 1
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.No_output)

let scope_overrides_production () =
  let console = ref 0 in
  let system =
    make_system
      ~write_console:(fun _ ->
        incr console;
        Observe.Platform.Accepted)
      ()
  in
  init_ok system (config ());
  let capture =
    match
      System.with_capture system (config ()) (fun capture ->
          Observe.Logs.info (Observe.Logs.text ~tag:"scope" "captured");
          Test_runtime.Runtime.return capture)
    with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "capture was rejected"
  in
  Alcotest.(check int) "scope suppresses console" 0 !console;
  Alcotest.(check int)
    "scope receives message" 1
    (List.length (Observe.Capture.logs capture));
  Observe.Logs.info (Observe.Logs.text ~tag:"production" "restored");
  Alcotest.(check int) "production restored" 1 !console

let nested_capture_precedence () =
  let console = ref 0 in
  let system =
    make_system
      ~write_console:(fun _ ->
        incr console;
        Observe.Platform.Accepted)
      ()
  in
  init_ok system (config ());
  let outer, inner =
    match
      System.with_capture system (config ()) (fun outer ->
          Observe.Logs.info (Observe.Logs.text ~tag:"outer" "before-inner");
          let inner =
            match
              System.with_capture system (config ()) (fun inner ->
                  Observe.Logs.info (Observe.Logs.text ~tag:"inner" "inside");
                  Test_runtime.Runtime.return inner)
            with
            | Ok inner -> inner
            | Error _ -> Alcotest.fail "nested capture was rejected"
          in
          Observe.Logs.info (Observe.Logs.text ~tag:"outer" "after-inner");
          Test_runtime.Runtime.return (outer, inner))
    with
    | Ok captures -> captures
    | Error _ -> Alcotest.fail "outer capture was rejected"
  in
  check_capture_tags "outer capture keeps only outer messages"
    [ "outer:before-inner"; "outer:after-inner" ]
    outer;
  check_capture_tags "inner capture takes precedence" [ "inner:inside" ] inner;
  Alcotest.(check int) "nested captures suppress production" 0 !console;
  Observe.Logs.info (Observe.Logs.text ~tag:"production" "after-captures");
  Alcotest.(check int) "production restored after nested captures" 1 !console

let cancellation_capture_close () =
  let context = Test_runtime.Inherited_runtime.create_context () in
  let console = ref 0 in
  let system =
    Inherited_system.create ~runtime_context:context
      ~platform:
        (Test_runtime.Platform.create
           ~write_console:(fun _ ->
             incr console;
             Observe.Platform.Accepted)
           ())
  in
  let capture = ref None in
  let inherited = ref None in
  let cancelled =
    try
      ignore
        (Inherited_system.with_capture system (inherited_config ())
           (fun current_capture ->
             capture := Some current_capture;
             inherited :=
               Some (Test_runtime.Inherited_runtime.inherited_context context);
             Observe.Logs.info (Observe.Logs.text ~tag:"cancel" "before-cancel");
             raise Test_runtime.Inherited_runtime.Cancelled));
      false
    with Test_runtime.Inherited_runtime.Cancelled -> true
  in
  Alcotest.(check bool) "native cancellation is preserved" true cancelled;
  Alcotest.(check int)
    "capture protect finalizer runs once" 1
    (Test_runtime.Inherited_runtime.finally_calls context);
  let capture =
    match !capture with
    | Some capture -> capture
    | None -> Alcotest.fail "cancellation did not enter capture"
  in
  check_capture_tags "cancelled capture retains admitted work"
    [ "cancel:before-cancel" ] capture;
  Observe.Logs.info
    (Observe.Logs.text ~tag:"restored" "before-production-install");
  Alcotest.(check int)
    "binding restored before production install" 1
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.Not_initialized);
  init_inherited_ok system (inherited_config ());
  let inherited =
    match !inherited with
    | Some inherited -> inherited
    | None -> Alcotest.fail "inherited context was not retained"
  in
  Test_runtime.Inherited_runtime.with_context context inherited (fun () ->
      Observe.Logs.info (Observe.Logs.text ~tag:"detached" "after-cancel");
      Observe.Logs.info (Observe.Logs.text ~tag:"detached" "after-cancel-again"));
  Alcotest.(check int)
    "closed cancelled capture withholds detached work" 0 !console;
  Alcotest.(check int)
    "each detached offer diagnoses the closed capture" 2
    (Test_runtime.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Capture_closed);
  Observe.Logs.info (Observe.Logs.text ~tag:"production" "restored");
  Alcotest.(check int) "production restored after cancellation" 1 !console

let closed_inherited_tombstone () =
  let context = Test_runtime.Inherited_runtime.create_context () in
  let console = ref 0 in
  let system =
    Inherited_system.create ~runtime_context:context
      ~platform:
        (Test_runtime.Platform.create
           ~write_console:(fun _ ->
             incr console;
             Observe.Platform.Accepted)
           ())
  in
  let inherited = ref None in
  let capture =
    match
      Inherited_system.with_capture system (inherited_config ())
        (fun current_capture ->
          inherited :=
            Some (Test_runtime.Inherited_runtime.inherited_context context);
          Observe.Logs.info (Observe.Logs.text ~tag:"capture" "before-detach");
          Test_runtime.Inherited_runtime.return current_capture)
    with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "capture was rejected"
  in
  init_inherited_ok system (inherited_config ());
  let inherited =
    match !inherited with
    | Some inherited -> inherited
    | None -> Alcotest.fail "inherited context was not retained"
  in
  Test_runtime.Inherited_runtime.with_context context inherited (fun () ->
      Observe.Logs.info (Observe.Logs.text ~tag:"detached" "closed-scope"));
  Alcotest.(check int)
    "closed inherited scope withholds from production" 0 !console;
  Alcotest.(check int)
    "closed inherited scope is diagnosed" 1
    (Test_runtime.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Capture_closed);
  Observe.Logs.info (Observe.Logs.text ~tag:"production" "after-detach");
  Alcotest.(check int) "root binding restored after detached work" 1 !console

let callback_restoration () =
  let console = ref 0 in
  let system =
    make_system
      ~write_console:(fun _ ->
        incr console;
        Observe.Platform.Accepted)
      ()
  in
  init_ok system (config ());
  Alcotest.check_raises "callback exception preserved" (Failure "callback")
    (fun () ->
      ignore
        (System.with_capture system (config ()) (fun _ ->
             raise (Failure "callback"))));
  Observe.Logs.info (Observe.Logs.text ~tag:"after" "production");
  Alcotest.(check int) "binding restored after exception" 1 !console

let control_exception () =
  let system = make_system () in
  init_ok system
    (config ~silent:true
       ~drains:[ Observe.Drain.create (fun _ -> Observe.Drain.Accepted) ]
       ());
  Alcotest.check_raises "runtime control exception preserved"
    Test_runtime.Runtime.Control (fun () ->
      Observe.Logs.info
        (Observe.Logs.free (fun () -> raise Test_runtime.Runtime.Control)))

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
  let system = make_system () in
  let actual =
    try
      ignore
        (System.with_capture system (config ~silent:true ()) (fun _ ->
             Observe.Logs.info
               (Observe.Logs.free (fun () ->
                    Printexc.raise_with_backtrace Test_runtime.Runtime.Control
                      original));
             Test_runtime.Runtime.return ()));
      None
    with Test_runtime.Runtime.Control -> Some (Printexc.get_raw_backtrace ())
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
  | "runtime-owner-conflict" -> runtime_owner_conflict
  | "invalid-capacity" -> invalid_capacity
  | "threshold-and-laziness" -> threshold_and_laziness
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
  | name -> fun () -> Alcotest.failf "unknown runtime scenario: %s" name

let () =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "missing" in
  (scenario name) ()
