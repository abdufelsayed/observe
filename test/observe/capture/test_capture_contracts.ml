module System =
  Observe.Runtime.Make (Test_runtime.Runtime) (Test_runtime.Platform)

let system =
  System.create ~runtime_context:() ~platform:(Test_runtime.Platform.create ())

let capture ?capacity config callback =
  match System.with_capture system config ?capacity callback with
  | Ok value -> value
  | Error Observe.Runtime.Runtime_already_registered ->
      Alcotest.fail "runtime unexpectedly conflicted"
  | Error (Observe.Runtime.Invalid_capacity capacity) ->
      Alcotest.failf "unexpected invalid capacity: %d" capacity

let expect_text ~tag ~message log =
  match Observe.Log.payload log with
  | Observe.Log.Text actual ->
      Alcotest.(check string) "tag" tag actual.tag;
      Alcotest.(check string) "message" message actual.message
  | Observe.Log.Free _ | Observe.Log.Structured _ ->
      Alcotest.fail "expected a text payload"

let test_payloads_and_metadata () =
  let config =
    Test_runtime.config ~environment:"test" ~version:"1"
      ~min_level:Observe.Level.Debug "capture"
  in
  let capture =
    capture config (fun capture ->
        Observe.Logs.info (Observe.Logs.text ~tag:"auth" "signed in");
        Observe.Logs.warn
          (Observe.Logs.free (fun () ->
               Observe.Value.object_
                 [
                   ("attempt", Observe.Value.int 2);
                   ("ok", Observe.Value.bool true);
                 ]));
        Observe.Logs.error (Observe.Logs.structured Observe.Type.int 7);
        capture)
  in
  match Observe.Capture.logs capture with
  | [ text; free; structured ] -> (
      Alcotest.(check string) "service" "capture" (Observe.Log.service text);
      Alcotest.(check (option string))
        "environment" (Some "test")
        (Observe.Log.environment text);
      Alcotest.(check (option string))
        "version" (Some "1") (Observe.Log.version text);
      Alcotest.(check int64)
        "instant" 42L
        (Observe.Log.instant text |> Observe.Instant.to_epoch_nanoseconds);
      Alcotest.(check bool)
        "level" true
        (Observe.Level.equal Observe.Level.Info (Observe.Log.level text));
      expect_text ~tag:"auth" ~message:"signed in" text;
      (match Observe.Log.payload free with
      | Observe.Log.Free value ->
          Alcotest.(check bool)
            "free payload readable" true
            (String.length (Observe.Value.to_string value) > 0)
      | Observe.Log.Text _ | Observe.Log.Structured _ ->
          Alcotest.fail "expected a free payload");
      match Observe.Log.payload structured with
      | Observe.Log.Structured (description, value) ->
          Alcotest.(check string)
            "structured value" "7"
            (Format.asprintf "%a" (Observe.Type.pp description) value)
      | Observe.Log.Text _ | Observe.Log.Free _ ->
          Alcotest.fail "expected a structured payload")
  | logs -> Alcotest.failf "expected three logs, received %d" (List.length logs)

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let test_formatter_semantics () =
  let config = Test_runtime.config ~pretty:false "formatter" in
  let log =
    capture config (fun capture ->
        Observe.Logs.info (Observe.Logs.text ~tag:"json" "hello");
        match Observe.Capture.logs capture with
        | [ log ] -> log
        | _ -> Alcotest.fail "expected one captured log")
  in
  let json =
    match Observe.Formatter.format Observe.Formatter.json log with
    | Ok json -> Yojson.Safe.from_string json
    | Error _ -> Alcotest.fail "JSON formatter rejected a valid text log"
  in
  Alcotest.(check bool)
    "service field" true
    (member "service" json = Some (`String "formatter"));
  Alcotest.(check bool)
    "level field" true
    (member "level" json = Some (`String "info"));
  let payload = member "payload" json in
  Alcotest.(check bool)
    "semantic text payload" true
    (match payload with
    | Some (`Assoc fields) ->
        List.assoc_opt "kind" fields = Some (`String "text")
        && List.assoc_opt "tag" fields = Some (`String "json")
        && List.assoc_opt "message" fields = Some (`String "hello")
    | _ -> false);
  (match Observe.Formatter.format Observe.Formatter.json_lines log with
  | Ok line ->
      Alcotest.(check char)
        "JSON Lines owns one final newline" '\n'
        line.[String.length line - 1];
      Alcotest.(check int)
        "exactly one newline" 1
        (String.fold_left
           (fun count character ->
             if character = '\n' then count + 1 else count)
           0 line)
  | Error _ -> Alcotest.fail "JSON Lines rejected a valid text log");
  let explicit =
    Observe.Formatter.create (fun _ -> Error Observe.Formatter.Failed)
  in
  Alcotest.(check bool)
    "explicit failure preserved" true
    (Observe.Formatter.format explicit log = Error Observe.Formatter.Failed);
  let raising = Observe.Formatter.create (fun _ -> failwith "formatter") in
  Alcotest.check_raises "direct formatter callback exceptions remain exceptions"
    (Failure "formatter") (fun () ->
      ignore (Observe.Formatter.format raising log))

let test_admission_laziness_and_diagnostics () =
  let config = Test_runtime.config ~min_level:Observe.Level.Info "admission" in
  let lazy_calls = ref 0 in
  let free_calls = ref 0 in
  let capture =
    capture ~capacity:1 config (fun capture ->
        Observe.Logs.debug
          (Observe.Logs.text_lazy ~tag:"lazy" (fun () ->
               incr lazy_calls;
               "rejected"));
        Observe.Logs.debug
          (Observe.Logs.free (fun () ->
               incr free_calls;
               Observe.Value.int 0));
        Observe.Logs.info
          (Observe.Logs.text_lazy ~tag:"lazy" (fun () ->
               incr lazy_calls;
               "accepted"));
        Observe.Logs.warn (Observe.Logs.text ~tag:"overflow" "withheld");
        Observe.Logs.error
          (Observe.Logs.free (fun () ->
               incr free_calls;
               failwith "authoring"));
        capture)
  in
  Alcotest.(check int) "lazy text forced only after admission" 1 !lazy_calls;
  Alcotest.(check int) "free thunk forced only after admission" 1 !free_calls;
  Alcotest.(check int)
    "capacity retains one" 1
    (List.length (Observe.Capture.logs capture));
  let diagnostics = Observe.Capture.diagnostics capture in
  Alcotest.(check int)
    "overflow diagnosed" 1
    (Test_runtime.diagnostic_count diagnostics
       Observe.Diagnostics.Capture_overflow);
  Alcotest.(check int)
    "authoring failure diagnosed" 1
    (Test_runtime.diagnostic_count diagnostics
       Observe.Diagnostics.Authoring_raised)

let test_structured_values_are_by_reference () =
  let value = ref 3 in
  let log =
    capture (Test_runtime.config "identity") (fun capture ->
        Observe.Logs.info
          (Observe.Logs.structured (Observe.Type.ref Observe.Type.int) value);
        match Observe.Capture.logs capture with
        | [ log ] -> log
        | _ -> Alcotest.fail "expected one structured log")
  in
  value := 9;
  match Observe.Log.payload log with
  | Observe.Log.Structured (description, retained) ->
      Alcotest.(check string)
        "later projection sees referenced value" "9"
        (Format.asprintf "%a" (Observe.Type.pp description) retained)
  | Observe.Log.Text _ | Observe.Log.Free _ ->
      Alcotest.fail "expected a structured payload"

let () =
  Alcotest.run "observe-capture-contracts"
    [
      ( "behavior:observe:capture",
        [
          Alcotest.test_case "payloads and metadata" `Quick
            test_payloads_and_metadata;
          Alcotest.test_case "formatter semantics" `Quick
            test_formatter_semantics;
          Alcotest.test_case "admission and diagnostics" `Quick
            test_admission_laziness_and_diagnostics;
          Alcotest.test_case "structured reference semantics" `Quick
            test_structured_values_are_by_reference;
        ] );
    ]
