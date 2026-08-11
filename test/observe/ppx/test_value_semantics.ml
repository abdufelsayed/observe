module System =
  Observe.Runtime.Make (Test_runtime.Runtime) (Test_runtime.Platform)

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let test_namespaced_value_is_deferred () =
  let forces = ref 0 in
  let value =
    [%observe.value
      {
        action = "user_login";
        active = true;
        attempts = [ 1; 2; 3 ];
        method_ = Some "oauth";
        previous = None;
        nested = { source = "web"; score = 1.5 };
        user_id =
          [%observe.value.embed
            Observe.Type.int,
            (incr forces;
             42)];
      }]
  in
  Alcotest.(check int) "extension constructs only a thunk" 0 !forces;
  let system =
    System.create ~runtime_context:()
      ~platform:(Test_runtime.Platform.create ())
  in
  let config = Test_runtime.config ~min_level:Observe.Level.Info "ppx-value" in
  let capture =
    match
      System.with_capture system config (fun capture ->
          Observe.Logs.debug (Observe.Logs.free value);
          Alcotest.(check int) "filtered value remains deferred" 0 !forces;
          Observe.Logs.info (Observe.Logs.free value);
          Test_runtime.Runtime.return capture)
    with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "capture rejected PPX value test"
  in
  Alcotest.(check int) "admitted value forced once" 1 !forces;
  let log =
    match Observe.Capture.logs capture with
    | [ log ] -> log
    | logs -> Alcotest.failf "expected one log, received %d" (List.length logs)
  in
  let json =
    match Observe.Formatter.format Observe.Formatter.json log with
    | Ok encoded -> Yojson.Safe.from_string encoded
    | Error _ -> Alcotest.fail "valid PPX value failed JSON formatting"
  in
  let payload = member "payload" json in
  Alcotest.(check bool)
    "semantic object shape" true
    (match payload with
    | Some (`Assoc fields) ->
        List.assoc_opt "action" fields = Some (`String "user_login")
        && List.assoc_opt "user_id" fields = Some (`Int 42)
        && List.assoc_opt "attempts" fields
           = Some (`List [ `Int 1; `Int 2; `Int 3 ])
    | _ -> false)

let () =
  Alcotest.run "observe-ppx-value"
    [
      ( "behavior:observe:ppx-value",
        [
          Alcotest.test_case "namespaced deferred object" `Quick
            test_namespaced_value_is_deferred;
        ] );
    ]
