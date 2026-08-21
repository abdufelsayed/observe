module Observer = Observe.Make (Test_io.IO)

let observer = Observer.create (Test_io.Host.create ())

type int_event = { value : int }

let int_event_t =
  let open Observe.Type in
  record "int_event" (fun value -> { value })
  |+ field "value" int (fun event -> event.value)
  |> sealr

type int_event_builder = {
  typed : int_event Observe.Schema.patch -> int_event Observe.Schema.patch;
}

let int_event_schema =
  Observe.Generated_runtime.record_schema int_event_t ~builder:(fun _ ->
      { typed = Fun.id })

type reference_event = { reference : int ref }

let reference_event_t =
  let open Observe.Type in
  record "reference_event" (fun reference -> { reference })
  |+ field "reference" (ref int) (fun event -> event.reference)
  |> sealr

type reference_event_builder = {
  typed :
    reference_event Observe.Schema.patch -> reference_event Observe.Schema.patch;
}

let reference_event_schema =
  Observe.Generated_runtime.record_schema reference_event_t ~builder:(fun _ ->
      { typed = Fun.id })

let structured_json log =
  match Observe.Log.body log with
  | Observe.Log.Text _ -> Alcotest.fail "expected a structured body"
  | Observe.Log.Structured { value; _ } ->
      Observe.Value.frozen_to_json_string value

let capture ?capacity config callback =
  match Observer.with_capture observer config ?capacity callback with
  | Ok value -> value
  | Error Observe.IO_already_registered ->
      Alcotest.fail "I/O implementation unexpectedly conflicted"
  | Error (Observe.Invalid_capacity capacity) ->
      Alcotest.failf "unexpected invalid capacity: %d" capacity

let expect_text ~tag ~message log =
  match Observe.Log.body log with
  | Observe.Log.Text actual ->
      Alcotest.(check string) "tag" tag actual.tag;
      Alcotest.(check string) "message" message actual.message
  | Observe.Log.Structured _ -> Alcotest.fail "expected a text body"

let test_bodies_and_metadata () =
  let config =
    Test_io.config ~environment:"test" ~version:"1"
      ~min_level:Observe.Level.Debug "capture"
  in
  let capture =
    capture config (fun capture ->
        Observe.Logs.info (Test_io.text ~tag:"auth" "signed in");
        Observe.Logs.warn (fun m ->
            m.value
              (Observe.Value.object_
                 [
                   ("attempt", Observe.Value.int 2);
                   ("ok", Observe.Value.bool true);
                 ]));
        Observe.Logs.error (fun m -> m.typed int_event_schema { value = 7 });
        capture)
  in
  match Observe.Capture.logs capture with
  | [ text; untyped; typed ] -> (
      Alcotest.(check string) "service" "capture" (Observe.Log.service text);
      Alcotest.(check (option string))
        "environment" (Some "test")
        (Observe.Log.environment text);
      Alcotest.(check (option string))
        "version" (Some "1") (Observe.Log.version text);
      Alcotest.(check int64)
        "timestamp" 42L
        (Observe.Log.timestamp text |> Observe.Timestamp.to_unix_ns);
      Alcotest.(check bool)
        "level" true
        (Observe.Level.equal Observe.Level.Info (Observe.Log.level text));
      expect_text ~tag:"auth" ~message:"signed in" text;
      Alcotest.(check string)
        "anonymous snapshot" "{\"attempt\":2,\"ok\":true}"
        (structured_json untyped);
      match Observe.Log.body typed with
      | Observe.Log.Structured { origin = Observe.Log.Declared "int_event"; _ }
        ->
          Alcotest.(check string)
            "declared snapshot" "{\"value\":7}" (structured_json typed)
      | Observe.Log.Text _ | Observe.Log.Structured _ ->
          Alcotest.fail "expected a declared typed body")
  | logs -> Alcotest.failf "expected three logs, received %d" (List.length logs)

let test_formatter_semantics () =
  let config = Test_io.config ~console:Observe.Config.Ndjson "formatter" in
  let log =
    capture config (fun capture ->
        Observe.Logs.info (Test_io.text ~tag:"json" "hello");
        match Observe.Capture.logs capture with
        | [ log ] -> log
        | _ -> Alcotest.fail "expected one captured log")
  in
  let json =
    match Observe.Formatter.format Observe.Formatter.json log with
    | Ok json -> json
    | Error _ -> Alcotest.fail "JSON formatter rejected a valid text log"
  in
  Alcotest.(check string)
    "semantic JSON envelope"
    "{\"service\":\"formatter\",\"timestamp\":\"42\",\"level\":\"info\",\"body\":{\"kind\":\"text\",\"tag\":\"json\",\"message\":\"hello\"}}"
    json;
  (match Observe.Formatter.format Observe.Formatter.ndjson log with
  | Ok line ->
      Alcotest.(check char)
        "NDJSON owns one final newline" '\n'
        line.[String.length line - 1];
      Alcotest.(check int)
        "exactly one newline" 1
        (String.fold_left
           (fun count character ->
             if character = '\n' then count + 1 else count)
           0 line)
  | Error _ -> Alcotest.fail "NDJSON rejected a valid text log");
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

let test_admission_and_diagnostics () =
  let config = Test_io.config ~min_level:Observe.Level.Info "admission" in
  let text_calls = ref 0 in
  let untyped_calls = ref 0 in
  let typed_calls = ref 0 in
  let capture =
    capture ~capacity:1 config (fun capture ->
        Observe.Logs.debug (fun m ->
            incr text_calls;
            m.text ~tag:"admission" "rejected");
        Observe.Logs.debug (fun m ->
            incr untyped_calls;
            m.value (Observe.Value.int 0));
        Observe.Logs.debug (fun m ->
            incr typed_calls;
            m.typed int_event_schema { value = 0 });
        Observe.Logs.info (fun m ->
            incr text_calls;
            m.text ~tag:"admission" "accepted");
        Observe.Logs.warn (Test_io.text ~tag:"overflow" "withheld");
        Observe.Logs.error (fun m ->
            incr untyped_calls;
            m.value (failwith "authoring"));
        capture)
  in
  Alcotest.(check int) "text authored only after admission" 1 !text_calls;
  Alcotest.(check int)
    "untyped value authored only after admission" 1 !untyped_calls;
  Alcotest.(check int) "filtered typed value not authored" 0 !typed_calls;
  Alcotest.(check int)
    "capacity retains one" 1
    (List.length (Observe.Capture.logs capture));
  let diagnostics = Observe.Capture.diagnostics capture in
  Alcotest.(check int)
    "overflow diagnosed" 1
    (Test_io.diagnostic_count diagnostics Observe.Diagnostics.Capture_overflow);
  Alcotest.(check int)
    "authoring failure diagnosed" 1
    (Test_io.diagnostic_count diagnostics
       Observe.Diagnostics.Message_evaluation_raised)

let test_typed_values_are_frozen () =
  let value = ref 3 in
  let log =
    capture (Test_io.config "identity") (fun capture ->
        Observe.Logs.info (fun m ->
            m.typed reference_event_schema { reference = value });
        match Observe.Capture.logs capture with
        | [ log ] -> log
        | _ -> Alcotest.fail "expected one typed log")
  in
  value := 9;
  Alcotest.(check string)
    "later mutation cannot change the snapshot" "{\"reference\":3}"
    (structured_json log)

let () =
  Alcotest.run "observe-capture-contracts"
    [
      ( "behavior:observe:capture",
        [
          Alcotest.test_case "bodies and metadata" `Quick
            test_bodies_and_metadata;
          Alcotest.test_case "formatter semantics" `Quick
            test_formatter_semantics;
          Alcotest.test_case "admission and diagnostics" `Quick
            test_admission_and_diagnostics;
          Alcotest.test_case "typed snapshot semantics" `Quick
            test_typed_values_are_frozen;
        ] );
    ]
