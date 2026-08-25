module Observer = Observe.Make (Test_io.IO)

let observer = Observer.create (Test_io.Host.create ())

let capture config callback =
  match
    Observer.with_capture observer ~config (fun capture ->
        callback ();
        capture)
  with
  | Ok capture -> capture
  | Error Observe.IO_already_registered ->
      Alcotest.fail "capture route unexpectedly conflicted"
  | Error (Observe.Invalid_capacity capacity) ->
      Alcotest.failf "unexpected invalid capture capacity: %d" capacity
  | Error Observe.Runtime_closed -> Alcotest.fail "runtime was closed"

let limits ?(max_depth = 100) ?(max_object_fields = 100)
    ?(max_collection_length = 100) ?(max_string_bytes = 10_000)
    ?(max_bytes_length = 10_000) ?(max_nodes = 10_000)
    ?(max_total_bytes = 1_000_000) () =
  Observe.Logs.Limits.create_exn ~max_depth ~max_object_fields
    ~max_collection_length ~max_string_bytes ~max_bytes_length ~max_nodes
    ~max_total_bytes ()

let config ?(limits = limits ()) service =
  Test_io.config ~console:Observe.Config.Silent ~limits service

let one_log capture =
  match Observe.Capture.logs capture with
  | [ log ] -> log
  | logs -> Alcotest.failf "expected one log, received %d" (List.length logs)

let fields log =
  match Observe.Value.view (Observe.Log.fields log) with
  | `Object fields -> fields
  | `Truncated_object (fields, _) -> fields
  | _ -> Alcotest.fail "expected an object field root"

let field name log =
  List.find_map
    (fun (candidate, value) ->
      if String.equal candidate name then Some value else None)
    (fields log)

let field_names log = List.map fst (fields log)

let json_fields log =
  Observe.Value.frozen_to_json_string (Observe.Log.fields log)

let check_no_marker value =
  let rec check value =
    match Observe.Value.view value with
    | `Truncated _ | `Truncated_list _ | `Truncated_object _ -> false
    | `List values -> List.for_all check values
    | `Object fields -> List.for_all (fun (_, value) -> check value) fields
    | `Variant (_, _, Some value) -> check value
    | `Variant (_, _, None)
    | `Null | `Bool _ | `Integer _ | `Float _ | `String _ | `Bytes _ ->
        true
  in
  check value

let check_int_field name expected log =
  match field name log with
  | Some value -> (
      match Observe.Value.view value with
      | `Integer (`Int actual) -> Alcotest.(check int) name expected actual
      | _ -> Alcotest.failf "field %s was not an int" name)
  | None -> Alcotest.failf "missing field %s in %s" name (json_fields log)

let check_string_field name expected log =
  match field name log with
  | Some value -> (
      match Observe.Value.view value with
      | `String actual -> Alcotest.(check string) name expected actual
      | _ -> Alcotest.failf "field %s was not a string" name)
  | None -> Alcotest.failf "missing field %s in %s" name (json_fields log)

let check_bytes_field name expected log =
  match field name log with
  | Some value -> (
      match Observe.Value.view value with
      | `Bytes actual -> Alcotest.(check string) name expected actual
      | _ -> Alcotest.failf "field %s was not bytes" name)
  | None -> Alcotest.failf "missing field %s in %s" name (json_fields log)

let test_exact_and_above_collection_and_object_limits () =
  let exact_collection =
    one_log
      (capture
         (config ~limits:(limits ~max_collection_length:2 ()) "exact-list")
         (fun () ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped
               |+ m.field "values" Observe.Type.(list int) [ 1; 2 ]
               |> m.seal)))
  in
  (match field "values" exact_collection with
  | Some value -> (
      match Observe.Value.view value with
      | `List [ _; _ ] -> ()
      | _ -> Alcotest.fail "an exact collection limit was marked truncated")
  | None -> Alcotest.fail "exact collection field was omitted");
  let above_collection =
    one_log
      (capture
         (config ~limits:(limits ~max_collection_length:2 ()) "above-list")
         (fun () ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped
               |+ m.field "values" Observe.Type.(list int) [ 1; 2; 3 ]
               |> m.seal)))
  in
  (match field "values" above_collection with
  | Some value -> (
      match Observe.Value.view value with
      | `Truncated_list ([ _; _ ], Observe.Value.Collection) -> ()
      | _ -> Alcotest.fail "an above-limit collection lost its safe prefix")
  | None -> Alcotest.fail "above-limit collection field was omitted");
  let exact_object =
    one_log
      (capture
         (config ~limits:(limits ~max_object_fields:2 ()) "exact-object")
         (fun () ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped
               |+ m.field "first" Observe.Type.int 1
               |+ m.field "second" Observe.Type.int 2
               |> m.seal)))
  in
  Alcotest.(check (list string))
    "exact object fields" [ "first"; "second" ] (field_names exact_object);
  let above_object =
    one_log
      (capture
         (config ~limits:(limits ~max_object_fields:2 ()) "above-object")
         (fun () ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped
               |+ m.field "first" Observe.Type.int 1
               |+ m.field "second" Observe.Type.int 2
               |+ m.field "third" Observe.Type.int 3
               |> m.seal)))
  in
  Alcotest.(check (list string))
    "above object safe prefix" [ "first"; "second" ] (field_names above_object);
  match Observe.Value.view (Observe.Log.fields above_object) with
  | `Truncated_object (_, Observe.Value.Object_fields) -> ()
  | _ -> Alcotest.fail "an above-limit object was not marked truncated"

let test_sequence_does_not_probe_beyond_collection_limit () =
  let calls = ref 0 in
  let rec values () =
    incr calls;
    if !calls > 2 then failwith "sequence tail was forced"
    else Seq.Cons (!calls, values)
  in
  let captured =
    capture
      (config ~limits:(limits ~max_collection_length:2 ()) "seq-lazy")
      (fun () ->
        Observe.Logs.info (fun m ->
            let open Observe.Logs in
            m.untyped
            |+ m.field "values" Observe.Type.(seq int) values
            |> m.seal))
  in
  let log = one_log captured in
  Alcotest.(check int) "sequence cells evaluated" 2 !calls;
  match field "values" log with
  | Some value -> (
      match Observe.Value.view value with
      | `Truncated_list ([ _; _ ], Observe.Value.Collection) -> ()
      | _ -> Alcotest.fail "sequence did not retain its bounded prefix")
  | None -> Alcotest.fail "sequence field was omitted"

type getter_record = { first : int; second : int; third : int }

let getter_type calls : getter_record Observe.Type.t =
  let open Observe.Type in
  record "getter_record" (fun first second third -> { first; second; third })
  |+ field "first" int (fun value -> value.first)
  |+ field "second" int (fun value -> value.second)
  |+ field "third" int (fun value ->
      incr calls;
      value.third)
  |> sealr

let test_typed_record_does_not_force_omitted_getters () =
  let calls = ref 0 in
  let description = getter_type calls in
  let schema = Observe.Schema.record description ~builder:(fun _ -> ()) in
  let record = { first = 1; second = 2; third = 3 } in
  let log =
    one_log
      (capture
         (config ~limits:(limits ~max_object_fields:2 ()) "getter-lazy")
         (fun () -> Observe.Logs.info (fun m -> m.typed ~using:schema record)))
  in
  Alcotest.(check int) "omitted typed getter calls" 0 !calls;
  Alcotest.(check (list string))
    "typed getter safe prefix" [ "first"; "second" ] (field_names log)

let test_many_small_strings_and_bytes_are_not_aggregate_limited () =
  let log =
    one_log
      (capture
         (config
            ~limits:
              (limits ~max_string_bytes:3 ~max_bytes_length:3
                 ~max_total_bytes:10_000 ())
            "svc")
         (fun () ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped
               |+ m.field "a" Observe.Type.string "abc"
               |+ m.field "b" Observe.Type.string "def"
               |+ m.field "c" Observe.Type.bytes (Bytes.of_string "ghi")
               |+ m.field "d" Observe.Type.bytes (Bytes.of_string "jkl")
               |> m.seal)))
  in
  check_string_field "a" "abc" log;
  check_string_field "b" "def" log;
  check_bytes_field "c" "ghi" log;
  check_bytes_field "d" "jkl" log;
  Alcotest.(check bool)
    "small values are not marked" true
    (check_no_marker (Observe.Log.fields log))

let test_total_bytes_is_an_aggregate_bound () =
  let limits = limits ~max_total_bytes:250 () in
  let log =
    one_log
      (capture (config ~limits "total-bytes") (fun () ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped
               |+ m.field "first" Observe.Type.string "one"
               |+ m.field "second" Observe.Type.string "two"
               |> m.seal)))
  in
  match Observe.Value.view (Observe.Log.fields log) with
  | `Truncated_object (fields, Observe.Value.Total_bytes) ->
      Alcotest.(check int)
        "aggregate bound keeps first field" 1 (List.length fields);
      Alcotest.(check bool)
        "aggregate bound keeps first name" true
        (List.exists (fun (name, _) -> String.equal name "first") fields)
  | _ -> Alcotest.fail "aggregate total-bytes bound did not localize"

let test_total_bytes_reserves_completed_log_metadata () =
  let limits = limits ~max_total_bytes:215 () in
  let point =
    one_log
      (capture (config ~limits "svc") (fun () ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped |+ m.field "first" Observe.Type.string "x" |> m.seal)))
  in
  let check label log =
    match Observe.Value.view (Observe.Log.fields log) with
    | `Truncated_object ([], Observe.Value.Total_bytes) -> ()
    | _ ->
        Alcotest.failf "aggregate total-bytes bound did not reserve %s metadata"
          label
  in
  check "point" point;
  let wide =
    one_log
      (capture (config ~limits "svc") (fun () ->
           let wide = Observe.Logs.create ~name:"o" () in
           Observe.Logs.set wide (fun m ->
               let open Observe.Logs in
               m.untyped |+ m.field "first" Observe.Type.string "x" |> m.seal);
           Observe.Logs.emit wide))
  in
  check "wide" wide

let test_single_field_container_failure_rolls_back_child () =
  let limits = limits ~max_total_bytes:63 () in
  let log =
    one_log
      (capture (config ~limits "svc") (fun () ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped |+ m.field "x" Observe.Type.int 1 |> m.seal)))
  in
  match Observe.Value.view (Observe.Log.fields log) with
  | `Truncated_object ([], Observe.Value.Total_bytes) -> ()
  | _ ->
      Alcotest.fail
        "single-field container overflow did not roll back its materialized \
         child"

type recursive_node = { child : recursive_node option }

let recursive_type : recursive_node Observe.Type.t =
  Observe.Type.mu (fun self ->
      let open Observe.Type in
      record "recursive_node" (fun child -> { child })
      |+ field "child" (option self) (fun node -> node.child)
      |> sealr)

let test_recursive_descriptions_charge_one_step () =
  let value = { child = Some { child = Some { child = None } } } in
  let schema = Observe.Schema.record recursive_type ~builder:(fun _ -> ()) in
  let log =
    one_log
      (capture
         (config ~limits:(limits ~max_nodes:3 ()) "recursive-steps")
         (fun () -> Observe.Logs.info (fun m -> m.typed ~using:schema value)))
  in
  Alcotest.(check bool)
    "recursive value fits one-step accounting" true
    (check_no_marker (Observe.Log.fields log));
  Alcotest.(check string)
    "recursive value shape" "{\"child\":{\"child\":{}}}" (json_fields log)

type sparse_record = { first : string; second : string; third : string }

let sparse_type : sparse_record Observe.Type.t =
  let open Observe.Type in
  record "sparse_record" (fun first second third -> { first; second; third })
  |+ field "first" string (fun value -> value.first)
  |+ field "second" string (fun value -> value.second)
  |+ field "third" string (fun value -> value.third)
  |> sealr

let test_sparse_typed_patches_do_not_charge_absent_fields () =
  let patch_builder = ref None in
  let schema =
    Observe.Schema.record sparse_type ~builder:(fun builder ->
        patch_builder := Some builder;
        ())
  in
  let patch name value =
    match !patch_builder with
    | Some builder ->
        Observe.Schema.field builder name Observe.Type.string value
    | None -> Alcotest.fail "schema builder was not initialized"
  in
  let log =
    one_log
      (capture
         (config ~limits:(limits ~max_object_fields:2 ()) "sparse-patches")
         (fun () ->
           let wide =
             Observe.Logs.create_typed ~name:"sparse" ~using:schema ()
           in
           Observe.Logs.set wide (fun () -> patch "first" "one");
           Observe.Logs.set wide (fun () -> patch "second" "two");
           Observe.Logs.emit wide))
  in
  Alcotest.(check (list string))
    "sparse patch fields" [ "first"; "second" ] (field_names log);
  Alcotest.(check bool)
    "sparse patch has no truncation" true
    (check_no_marker (Observe.Log.fields log))

let test_wide_truncation_preserves_committed_fields () =
  let captured =
    capture
      (config ~limits:(limits ~max_object_fields:2 ()) "wide-boundary")
      (fun () ->
        let wide = Observe.Logs.create ~name:"wide-boundary" () in
        Observe.Logs.set wide (fun m ->
            let open Observe.Logs in
            m.untyped |+ m.field "first" Observe.Type.int 1 |> m.seal);
        Observe.Logs.set wide (fun m ->
            let open Observe.Logs in
            m.untyped |+ m.field "second" Observe.Type.int 2 |> m.seal);
        Observe.Logs.set wide (fun m ->
            let open Observe.Logs in
            m.untyped |+ m.field "third" Observe.Type.int 3 |> m.seal);
        Observe.Logs.emit wide)
  in
  let log = one_log captured in
  Alcotest.(check (list string))
    "wide safe fields survive overflow" [ "first"; "second" ] (field_names log);
  (match Observe.Value.view (Observe.Log.fields log) with
  | `Truncated_object (_, Observe.Value.Object_fields) -> ()
  | _ -> Alcotest.fail "wide overflow lost its object-fields marker");
  check_int_field "first" 1 log;
  check_int_field "second" 2 log

let test_nested_enrichment_merges_through_public_api () =
  let enricher =
    Observe.Logs.Enricher.create_exn ~name:"nested" (fun () ->
        Observe.Value.object_
          [
            ( "context",
              Observe.Value.object_
                [
                  ("enriched", Observe.Value.string "yes");
                  ("caller", Observe.Value.string "enricher");
                ] );
          ])
  in
  let log =
    one_log
      (capture
         (Test_io.config ~console:Observe.Config.Silent ~enrichers:[ enricher ]
            "nested-enrichment") (fun () ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped
               |+ m.object_ "context" (fun n ->
                   n.untyped
                   |+ n.field "caller" Observe.Type.string "caller"
                   |> n.seal)
               |> m.seal)))
  in
  match field "context" log with
  | Some value -> (
      match Observe.Value.view value with
      | `Object nested -> (
          let names = List.map fst nested in
          Alcotest.(check (list string))
            "nested enrichment fields" [ "caller"; "enriched" ] names;
          let caller = List.assoc "caller" nested in
          let enriched = List.assoc "enriched" nested in
          (match Observe.Value.view caller with
          | `String value ->
              Alcotest.(check string) "nested caller wins" "caller" value
          | _ -> Alcotest.fail "nested caller was not a string");
          match Observe.Value.view enriched with
          | `String value ->
              Alcotest.(check string) "nested enrichment" "yes" value
          | _ -> Alcotest.fail "nested enrichment was not a string")
      | _ -> Alcotest.fail "nested enrichment did not produce an object")
  | None -> Alcotest.fail "nested context field was omitted"

let () =
  Alcotest.run "observe-materialization-boundaries"
    [
      ( "contracts",
        [
          Alcotest.test_case "exact and above collection/object limits" `Quick
            test_exact_and_above_collection_and_object_limits;
          Alcotest.test_case "sequence limit does not probe its tail" `Quick
            test_sequence_does_not_probe_beyond_collection_limit;
          Alcotest.test_case "typed record limit does not force getters" `Quick
            test_typed_record_does_not_force_omitted_getters;
          Alcotest.test_case "small strings and bytes are individually bounded"
            `Quick test_many_small_strings_and_bytes_are_not_aggregate_limited;
          Alcotest.test_case "total bytes is aggregate" `Quick
            test_total_bytes_is_an_aggregate_bound;
          Alcotest.test_case "total bytes reserves completed-log metadata"
            `Quick test_total_bytes_reserves_completed_log_metadata;
          Alcotest.test_case "single-field container rollback" `Quick
            test_single_field_container_failure_rolls_back_child;
          Alcotest.test_case "recursive descriptions charge one step" `Quick
            test_recursive_descriptions_charge_one_step;
          Alcotest.test_case "sparse typed patches charge authored fields"
            `Quick test_sparse_typed_patches_do_not_charge_absent_fields;
          Alcotest.test_case "wide truncation preserves committed fields" `Quick
            test_wide_truncation_preserves_committed_fields;
          Alcotest.test_case "nested enrichment is caller-wins" `Quick
            test_nested_enrichment_merges_through_public_api;
        ] );
    ]
