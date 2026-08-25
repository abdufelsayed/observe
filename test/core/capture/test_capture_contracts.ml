module Observer = Observe.Make (Test_io.IO)

let point_correlation log =
  match Observe.Log.kind log with
  | Observe.Log.Point { correlation } -> correlation
  | Observe.Log.Wide _ -> Alcotest.fail "expected point log"

let wide_operation log =
  match Observe.Log.kind log with
  | Observe.Log.Wide { operation; _ } -> operation
  | Observe.Log.Point _ -> Alcotest.fail "expected wide log"

let take label values =
  match !values with
  | value :: rest ->
      values := rest;
      value
  | [] -> Alcotest.failf "%s fixture was exhausted" label

let monotonic = ref []
let identities = ref []

let observer =
  Observer.create
    (Test_io.Host.create
       ~monotonic_now:(fun () -> Ok (take "monotonic" monotonic))
       ~next_id:(fun () -> Ok (take "identity" identities))
       ())

type int_event = { value : int }

let int_event_t =
  let open Observe.Type in
  record "int_event" (fun value -> { value })
  |+ field "value" int (fun event -> event.value)
  |> sealr

type int_event_builder = {
  typed : int_event Observe.Schema.patch -> int_event Observe.Schema.patch;
}

let int_event_patch_builder = ref None

let int_event_schema =
  Observe.Schema.record int_event_t ~builder:(fun patch_builder ->
      int_event_patch_builder := Some patch_builder;
      { typed = Fun.id })

let int_event_patch ?value () =
  let patch_builder =
    match !int_event_patch_builder with
    | Some patch_builder -> patch_builder
    | None -> Alcotest.fail "int event schema builder was not initialized"
  in
  match value with
  | None -> Observe.Schema.combine patch_builder []
  | Some value ->
      Observe.Schema.field patch_builder "value" Observe.Type.int value

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
  Observe.Schema.record reference_event_t ~builder:(fun _ -> { typed = Fun.id })

let structured_json log =
  match Observe.Log.event log with
  | Observe.Log.Text _ -> Alcotest.fail "expected a structured body"
  | Observe.Log.Structured { value; _ } ->
      Observe.Value.frozen_to_json_string value

let capture ?capacity config callback =
  match Observer.with_capture observer ~config ?capacity callback with
  | Ok value -> value
  | Error Observe.IO_already_registered ->
      Alcotest.fail "I/O implementation unexpectedly conflicted"
  | Error (Observe.Invalid_capacity capacity) ->
      Alcotest.failf "unexpected invalid capacity: %d" capacity
  | Error Observe.Runtime_closed -> Alcotest.fail "runtime was already closed"

let expect_text ~tag ~message log =
  match Observe.Log.event log with
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
            let open Observe.Logs in
            m.untyped
            |+ m.field "attempt" Observe.Type.int 2
            |+ m.field "ok" Observe.Type.bool true
            |> m.seal);
        Observe.Logs.error (fun m ->
            m.typed ~using:int_event_schema { value = 7 });
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
      (match Observe.Log.event untyped with
      | Observe.Log.Structured { value; _ } -> (
          match Observe.Value.view value with
          | `Object [ ("attempt", attempt); ("ok", ok) ] ->
              Alcotest.(check bool)
                "semantic integer view" true
                (Observe.Value.view attempt = `Integer (`Int 2));
              Alcotest.(check bool)
                "semantic Boolean view" true
                (Observe.Value.view ok = `Bool true)
          | _ -> Alcotest.fail "unexpected semantic object view")
      | Observe.Log.Text _ -> Alcotest.fail "expected structured view");
      match Observe.Log.event typed with
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
    "semantic flat JSON event"
    "{\"service\":\"formatter\",\"timestamp\":\"1970-01-01T00:00:00.000000042Z\",\"level\":\"info\",\"tag\":\"json\",\"message\":\"hello\"}"
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
            let open Observe.Logs in
            m.untyped |+ m.field "value" Observe.Type.int 0 |> m.seal);
        Observe.Logs.debug (fun m ->
            incr typed_calls;
            m.typed ~using:int_event_schema { value = 0 });
        Observe.Logs.info (fun m ->
            incr text_calls;
            m.text ~tag:"admission" "accepted");
        Observe.Logs.warn (Test_io.text ~tag:"overflow" "withheld");
        Observe.Logs.error (fun m ->
            incr untyped_calls;
            ignore m;
            failwith "authoring");
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
            m.typed ~using:reference_event_schema { reference = value });
        match Observe.Capture.logs capture with
        | [ log ] -> log
        | _ -> Alcotest.fail "expected one typed log")
  in
  value := 9;
  Alcotest.(check string)
    "later mutation cannot change the snapshot" "{\"reference\":3}"
    (structured_json log)

let test_duplicate_fields_are_rejected_per_contribution () =
  identities := [ "op-replace"; "op-duplicate" ];
  monotonic := [ 0L; 1L; 2L; 3L ];
  let capture =
    capture (Test_io.config "duplicate-fields") (fun capture ->
        Observe.Logs.info (fun m ->
            let open Observe.Logs in
            m.untyped
            |+ m.field "duplicate" Observe.Type.int 1
            |+ m.field "duplicate" Observe.Type.int 2
            |> m.seal);
        Observe.Logs.info (fun m ->
            let open Observe.Logs in
            m.untyped
            |+ m.field "duplicate" Observe.Type.int 1
            |+ m.field "duplicate" Observe.Type.int 2
            |> m.seal);
        Observe.Logs.info (fun m ->
            let open Observe.Logs in
            m.untyped
            |+ m.object_ "nested" (fun n ->
                n.untyped
                |+ n.field "duplicate" Observe.Type.int 1
                |+ n.field "duplicate" Observe.Type.int 2
                |> n.seal)
            |> m.seal);
        let replacement = Observe.Logs.create ~name:"replacement" () in
        Observe.Logs.set replacement (fun m ->
            let open Observe.Logs in
            m.untyped |+ m.field "phase" Observe.Type.string "started" |> m.seal);
        Observe.Logs.set replacement (fun m ->
            let open Observe.Logs in
            m.untyped
            |+ m.field "phase" Observe.Type.string "finished"
            |> m.seal);
        Observe.Logs.emit replacement;
        let duplicate = Observe.Logs.create ~name:"duplicate" () in
        Observe.Logs.set duplicate (fun m ->
            let open Observe.Logs in
            m.untyped
            |+ m.field "field" Observe.Type.int 1
            |+ m.field "field" Observe.Type.int 2
            |> m.seal);
        Observe.Logs.emit duplicate;
        capture)
  in
  (match Observe.Capture.logs capture with
  | [ replacement ] ->
      Alcotest.(check string)
        "later contributions replace prior fields" "{\"phase\":\"finished\"}"
        (structured_json replacement)
  | logs ->
      Alcotest.failf "expected one valid replacement, received %d logs"
        (List.length logs));
  Alcotest.(check int)
    "every duplicate contribution was diagnosed" 4
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Canonical_freeze_failed)

let test_typed_patches_require_schema_identity () =
  identities := [ "op-schema-mismatch" ];
  monotonic := [ 0L; 1L ];
  let same_named_schema =
    Observe.Schema.record int_event_t ~builder:(fun _ ->
        ({ typed = Fun.id } : int_event_builder))
  in
  let capture =
    capture (Test_io.config "schema-identity") (fun capture ->
        let wide =
          Observe.Logs.create_typed ~name:"schema-identity"
            ~using:same_named_schema ()
        in
        Observe.Logs.set wide (fun m -> m.typed (int_event_patch ~value:7 ()));
        Observe.Logs.emit wide;
        capture)
  in
  Alcotest.(check int)
    "same-named foreign schema patch was withheld" 0
    (List.length (Observe.Capture.logs capture));
  Alcotest.(check int)
    "schema mismatch was diagnosed" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Canonical_freeze_failed)

let test_point_and_wide_semantic_capture () =
  monotonic := [ 0L; 10L; 71_000_010L; 184_000_000L ];
  identities := [ "op_parent"; "op_child" ];
  let logs =
    capture (Test_io.config ~environment:"test" ~version:"2" "capture-wide")
      (fun capture ->
        Observe.Logs.info (Test_io.text ~tag:"point" "ordinary");
        let parent = Observe.Logs.create ~name:"checkout" () in
        let child =
          Observe.Logs.create_typed ~parent ~name:"validate-cart"
            ~using:int_event_schema ()
        in
        Observe.Logs.info ~operation:parent
          (Test_io.text ~tag:"correlated" "waiting");
        Observe.Logs.set parent (fun m ->
            let open Observe.Logs in
            m.untyped
            |+ m.field "cart_id" Observe.Type.string "cart-1"
            |> m.seal);
        Observe.Logs.set child (fun m -> m.typed (int_event_patch ~value:7 ()));
        Observe.Logs.emit child;
        Observe.Logs.emit parent;
        Observe.Capture.logs capture)
  in
  match logs with
  | [ point; correlated; child; parent ] ->
      Alcotest.(check (option string))
        "ordinary point has no correlation" None
        (Option.map Observe.Log.operation_reference_id (point_correlation point));
      Alcotest.(check (option string))
        "correlated point identity" (Some "op_parent")
        (Option.map Observe.Log.operation_reference_id
           (point_correlation correlated));
      let child_operation = wide_operation child in
      Alcotest.(check string)
        "child name" "validate-cart"
        (Observe.Log.operation_name child_operation);
      Alcotest.(check string)
        "child identity" "op_child"
        (Observe.Log.operation_id child_operation);
      Alcotest.(check (option string))
        "child parent" (Some "op_parent")
        (Option.map Observe.Log.operation_reference_id
           (Observe.Log.operation_parent child_operation));
      Alcotest.(check int64)
        "child duration" 71_000_000L
        (Observe.Log.operation_duration_ns child_operation);
      (match Observe.Log.event child with
      | Observe.Log.Structured
          { origin = Observe.Log.Declared "int_event"; value } ->
          Alcotest.(check string)
            "sparse declared snapshot" "{\"value\":7}"
            (Observe.Value.frozen_to_json_string value)
      | Observe.Log.Text _ | Observe.Log.Structured _ ->
          Alcotest.fail "expected declared child body");
      let parent_operation = wide_operation parent in
      Alcotest.(check string)
        "parent name" "checkout"
        (Observe.Log.operation_name parent_operation);
      Alcotest.(check string)
        "parent identity" "op_parent"
        (Observe.Log.operation_id parent_operation);
      Alcotest.(check (option string))
        "parent has no parent" None
        (Option.map Observe.Log.operation_reference_id
           (Observe.Log.operation_parent parent_operation));
      Alcotest.(check int64)
        "parent duration" 184_000_000L
        (Observe.Log.operation_duration_ns parent_operation);
      Alcotest.(check string)
        "independent parent body" "{\"cart_id\":\"cart-1\"}"
        (structured_json parent);
      List.iter
        (fun log ->
          Alcotest.(check int64)
            "controlled completion timestamp" 42L
            (Observe.Log.timestamp log |> Observe.Timestamp.to_unix_ns))
        logs
  | logs ->
      Alcotest.failf "expected four captured observations, received %d"
        (List.length logs)

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
          Alcotest.test_case "duplicate fields" `Quick
            test_duplicate_fields_are_rejected_per_contribution;
          Alcotest.test_case "typed schema identity" `Quick
            test_typed_patches_require_schema_identity;
          Alcotest.test_case "point and wide semantic inspection" `Quick
            test_point_and_wide_semantic_capture;
        ] );
    ]
