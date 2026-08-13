type event = User_login of { user_id : int; method_ : string }
[@@deriving observe]

module Observer = Observe.Make (Test_io.IO)

let level = Alcotest.testable Observe.Level.pp Observe.Level.equal

let contains value fragment =
  let value_length = String.length value in
  let fragment_length = String.length fragment in
  let rec loop index =
    if index + fragment_length > value_length then false
    else if String.sub value index fragment_length = fragment then true
    else loop (index + 1)
  in
  fragment_length = 0 || loop 0

let test_logging_extensions_lower_to_admitted_builders () =
  let rejected_evaluations = ref 0 in
  let observer = Observer.create (Test_io.Host.create ()) in
  let config =
    Test_io.config ~min_level:Observe.Level.Info ~console:Observe.Config.Silent
      "ppx-logging"
  in
  let capture =
    match
      Observer.with_capture observer config (fun capture ->
          [%observe.debug
            text ~tag:"filtered" "%d"
              (incr rejected_evaluations;
               1)];
          [%observe.info text ~tag:"auth" "user %d logged in" 42];
          [%observe.warn
            untyped [%observe.value { action = "retry"; attempt = 2 }]];
          [%observe.emit
            Observe.Level.Error,
            typed event_t (User_login { user_id = 42; method_ = "oauth" })];
          Test_io.Direct.return capture)
    with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "capture rejected logging PPX test"
  in
  Alcotest.(check int)
    "filtered logging body remains deferred" 0 !rejected_evaluations;
  let logs = Observe.Capture.logs capture in
  Alcotest.(check int) "three admitted logs" 3 (List.length logs);
  match logs with
  | [ text_log; untyped_log; typed_log ] -> (
      Alcotest.check level "text level" Observe.Level.Info
        (Observe.Log.level text_log);
      Alcotest.check level "untyped level" Observe.Level.Warn
        (Observe.Log.level untyped_log);
      Alcotest.check level "dynamic typed level" Observe.Level.Error
        (Observe.Log.level typed_log);
      (match Observe.Log.body text_log with
      | Observe.Log.Text { tag; message } ->
          Alcotest.(check string) "text tag" "auth" tag;
          Alcotest.(check string) "formatted text" "user 42 logged in" message
      | Observe.Log.Untyped _ | Observe.Log.Typed _ ->
          Alcotest.fail "text extension produced the wrong body");
      (match Observe.Log.body untyped_log with
      | Observe.Log.Untyped
          (Observe.Value.Object
             [
               ("action", Observe.Value.String action);
               ("attempt", Observe.Value.Int attempt);
             ]) ->
          Alcotest.(check string) "untyped action" "retry" action;
          Alcotest.(check int) "untyped attempt" 2 attempt
      | Observe.Log.Text _ | Observe.Log.Typed _ | Observe.Log.Untyped _ ->
          Alcotest.fail "untyped extension produced the wrong body");
      match Observe.Formatter.format Observe.Formatter.json typed_log with
      | Ok json ->
          Alcotest.(check bool)
            "typed value retained" true
            (contains json
               "\"body\":{\"User_login\":{\"user_id\":42,\"method_\":\"oauth\"}}")
      | Error _ -> Alcotest.fail "typed extension failed JSON formatting")
  | _ -> Alcotest.fail "capture order changed"

let () =
  Alcotest.run "observe-ppx-logging"
    [
      ( "behavior:observe:ppx-logging",
        [
          Alcotest.test_case "admission and all body forms" `Quick
            test_logging_extensions_lower_to_admitted_builders;
        ] );
    ]
