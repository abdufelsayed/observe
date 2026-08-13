module Observer = Observe.Make (Test_io.IO)

let contains value fragment =
  let value_length = String.length value in
  let fragment_length = String.length fragment in
  let rec loop index =
    if index + fragment_length > value_length then false
    else if String.sub value index fragment_length = fragment then true
    else loop (index + 1)
  in
  fragment_length = 0 || loop 0

let test_namespaced_value_respects_admission () =
  let forces = ref 0 in
  let author (m : Observe.Logs.builder) =
    m.untyped
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
  Alcotest.(check int) "author has not run" 0 !forces;
  let observer = Observer.create (Test_io.Host.create ()) in
  let config = Test_io.config ~min_level:Observe.Level.Info "ppx-value" in
  let capture =
    match
      Observer.with_capture observer config (fun capture ->
          Observe.Logs.debug author;
          Alcotest.(check int) "filtered value remains deferred" 0 !forces;
          Observe.Logs.info author;
          Test_io.Direct.return capture)
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
    | Ok encoded -> encoded
    | Error _ -> Alcotest.fail "valid PPX value failed JSON formatting"
  in
  Alcotest.(check bool)
    "semantic object shape" true
    (contains json "\"body\":{\"action\":\"user_login\""
    && contains json "\"user_id\":42"
    && contains json "\"attempts\":[1,2,3]")

let () =
  Alcotest.run "observe-ppx-value"
    [
      ( "behavior:observe:ppx-value",
        [
          Alcotest.test_case "namespaced admitted object" `Quick
            test_namespaced_value_respects_admission;
        ] );
    ]
