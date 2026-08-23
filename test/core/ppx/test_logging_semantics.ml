type event = { user_id : int; method_ : string } [@@deriving observe]
type 'a box = { value : 'a } [@@deriving observe]

module Observer = Observe.Make (Test_io.IO)

let observer = Observer.create (Test_io.Host.create ())
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
  let config =
    Test_io.config ~min_level:Observe.Level.Info ~console:Observe.Config.Silent
      "ppx-logging"
  in
  let capture =
    match
      Observer.with_capture observer ~config (fun capture ->
          [%observe.debug
            text ~tag:"filtered" "%d"
              (incr rejected_evaluations;
               1)];
          [%observe.info text ~tag:"auth" "user %d logged in" 42];
          let request_id = "request-7" in
          [%observe.warn
            untyped
              {
                action = "retry";
                attempt = 2;
                request_id = string request_id;
                context = { cached = false; roles = [ "admin"; "billing" ] };
                referral = Some "partner";
                mixed = [ 1; "two"; false ];
              }];
          Observe.Logs.log ~level:Observe.Level.Error (fun m ->
              m.typed ~using:event_schema { user_id = 42; method_ = "oauth" });
          [%observe.error
            error ~using:Observe.Error.exn ~backtrace:(Printexc.get_callstack 4)
              (Failure "point")];
          Test_io.Direct.return capture)
    with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "capture rejected logging PPX test"
  in
  Alcotest.(check int)
    "filtered logging body remains deferred" 0 !rejected_evaluations;
  let logs = Observe.Capture.logs capture in
  Alcotest.(check int) "four admitted logs" 4 (List.length logs);
  match logs with
  | [ text_log; untyped_log; typed_log; error_log ] -> (
      Alcotest.check level "text level" Observe.Level.Info
        (Observe.Log.level text_log);
      Alcotest.check level "untyped level" Observe.Level.Warn
        (Observe.Log.level untyped_log);
      Alcotest.check level "dynamic typed level" Observe.Level.Error
        (Observe.Log.level typed_log);
      (match Observe.Log.event text_log with
      | Observe.Log.Text { tag; message } ->
          Alcotest.(check string) "text tag" "auth" tag;
          Alcotest.(check string) "formatted text" "user 42 logged in" message
      | Observe.Log.Structured _ ->
          Alcotest.fail "text extension produced the wrong body");
      (match Observe.Log.event untyped_log with
      | Observe.Log.Structured { value; _ } ->
          Alcotest.(check string)
            "anonymous body"
            "{\"action\":\"retry\",\"attempt\":2,\"request_id\":\"request-7\",\"context\":{\"cached\":false,\"roles\":[\"admin\",\"billing\"]},\"referral\":\"partner\",\"mixed\":[1,\"two\",false]}"
            (Observe.Value.frozen_to_json_string value)
      | Observe.Log.Text _ ->
          Alcotest.fail "untyped extension produced the wrong body");
      (match Observe.Log.event typed_log with
      | Observe.Log.Structured
          { origin = Observe.Log.Declared "Test_logging_semantics.event"; _ } ->
          ()
      | Observe.Log.Text _ | Observe.Log.Structured _ ->
          Alcotest.fail "generated schema identity is not source-qualified");
      (match Observe.Formatter.format Observe.Formatter.json typed_log with
      | Ok json ->
          Alcotest.(check bool)
            "typed value retained" true
            (contains json "\"user_id\":42,\"method_\":\"oauth\"")
      | Error _ -> Alcotest.fail "typed extension failed JSON formatting");
      Alcotest.check level "explicit point error level" Observe.Level.Error
        (Observe.Log.level error_log);
      match Observe.Log.event error_log with
      | Observe.Log.Structured { value; _ } ->
          Alcotest.(check bool)
            "explicit point error is structured" true
            (String.starts_with ~prefix:"{\"error\":"
               (Observe.Value.frozen_to_json_string value))
      | Observe.Log.Text _ -> Alcotest.fail "explicit point error produced text"
      )
  | _ -> Alcotest.fail "capture order changed"

let test_parameterized_patch_uses_exact_schema () =
  let using = box_schema Observe.Type.int in
  let capture =
    match
      Observer.with_capture observer ~config:(Test_io.config "parameterized")
        (fun capture ->
          let wide = Observe.Logs.create_typed ~name:"box" ~using () in
          Observe.Logs.set wide (fun m ->
              m.typed (box_patch ~using ~value:42 ()));
          Observe.Logs.emit wide;
          Test_io.Direct.return capture)
    with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "capture rejected parameterized schema test"
  in
  match Observe.Capture.logs capture with
  | [ log ] ->
      Alcotest.(check string)
        "parameterized patch" "{\"value\":42}"
        (match Observe.Log.event log with
        | Observe.Log.Structured { value; _ } ->
            Observe.Value.frozen_to_json_string value
        | Observe.Log.Text _ -> Alcotest.fail "expected structured wide log")
  | logs ->
      Alcotest.failf "expected one parameterized wide log, received %d"
        (List.length logs)

let () =
  Alcotest.run "observe-ppx-logging"
    [
      ( "behavior:observe:ppx-logging",
        [
          Alcotest.test_case "admission and all body forms" `Quick
            test_logging_extensions_lower_to_admitted_builders;
          Alcotest.test_case "parameterized sparse patch identity" `Quick
            test_parameterized_patch_uses_exact_schema;
        ] );
    ]
