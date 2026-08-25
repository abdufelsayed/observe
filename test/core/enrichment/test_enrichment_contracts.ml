module Observer = Observe.Make (Test_io.IO)

let observer = Observer.create (Test_io.Host.create ())

let capture config callback =
  match
    Observer.with_capture observer ~config (fun capture ->
        callback capture;
        capture)
  with
  | Ok capture -> capture
  | Error Observe.IO_already_registered ->
      Alcotest.fail "capture route unexpectedly conflicted"
  | Error (Observe.Invalid_capacity capacity) ->
      Alcotest.failf "unexpected invalid capture capacity: %d" capacity
  | Error Observe.Runtime_closed -> Alcotest.fail "runtime was closed"

let json_fields log =
  Observe.Value.frozen_to_json_string (Observe.Log.fields log)

let object_fields log =
  match Observe.Value.view (Observe.Log.fields log) with
  | `Object fields -> fields
  | _ -> Alcotest.fail "expected an object field root"

let has_field name log =
  List.exists
    (fun (candidate, _) -> String.equal name candidate)
    (object_fields log)

let field_value name log =
  List.find_map
    (fun (candidate, value) ->
      if String.equal name candidate then Some value else None)
    (object_fields log)

let check_has_field name log =
  Alcotest.(check bool) ("field " ^ name) true (has_field name log)

let check_string_field name expected log =
  match field_value name log with
  | Some value -> (
      match Observe.Value.view value with
      | `String actual ->
          Alcotest.(check string) ("field " ^ name) expected actual
      | _ -> Alcotest.failf "field %s was not a string" name)
  | None -> Alcotest.failf "missing field %s in %s" name (json_fields log)

let check_truncation name expected log =
  match field_value name log with
  | Some value -> (
      match Observe.Value.view value with
      | `Truncated actual ->
          Alcotest.(check bool) ("truncation " ^ name) true (actual = expected)
      | `Truncated_list (_, actual) | `Truncated_object (_, actual) ->
          Alcotest.(check bool) ("truncation " ^ name) true (actual = expected)
      | _ -> Alcotest.failf "field %s was not a truncation marker" name)
  | None -> Alcotest.failf "missing field %s in %s" name (json_fields log)

let json log =
  match Observe.Formatter.format Observe.Formatter.json log with
  | Ok value -> value
  | Error _ -> Alcotest.fail "JSON formatter rejected a completed log"

let pretty log =
  match
    Observe.Formatter.format
      (Observe.Formatter.pretty Observe.Formatter.Plain)
      log
  with
  | Ok value -> value
  | Error _ -> Alcotest.fail "pretty formatter rejected a completed log"

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search offset =
    if offset + fragment_length > text_length then false
    else if String.equal (String.sub text offset fragment_length) fragment then
      true
    else search (offset + 1)
  in
  fragment_length = 0 || search 0

let rec contains_marker value =
  match Observe.Value.view value with
  | `Truncated _ -> true
  | `Truncated_list _ | `Truncated_object _ -> true
  | `Object fields ->
      List.exists (fun (_, value) -> contains_marker value) fields
  | `List values -> List.exists contains_marker values
  | `Variant (_, _, Some value) -> contains_marker value
  | `Variant (_, _, None)
  | `Null | `Bool _ | `Integer _ | `Float _ | `String _ | `Bytes _ ->
      false

let make_enricher ?(authoritative_fields = []) name fields =
  Observe.Logs.Enricher.create_exn ~name ~authoritative_fields (fun () ->
      Observe.Value.object_ fields)

type typed_event = { caller : string }

type typed_event_builder = {
  typed : typed_event Observe.Schema.patch -> typed_event Observe.Schema.patch;
}

let typed_event_t =
  let open Observe.Type in
  record "enrichment_typed_event" (fun caller -> { caller })
  |+ field "caller" string (fun event -> event.caller)
  |> sealr

let typed_event_patch_builder = ref None

let typed_event_schema =
  Observe.Schema.record typed_event_t ~builder:(fun patch_builder ->
      typed_event_patch_builder := Some patch_builder;
      { typed = Fun.id })

let typed_event_patch caller =
  match !typed_event_patch_builder with
  | Some patch_builder ->
      Observe.Schema.field patch_builder "caller" Observe.Type.string caller
  | None -> Alcotest.fail "typed event schema builder was not initialized"

let open_point_author caller (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped |+ m.field "caller" Observe.Type.string caller |> m.seal

let typed_point_author caller (m : Observe.Logs.builder) =
  m.typed ~using:typed_event_schema { caller }

let open_wide_author caller (m : Observe.Logs.untyped_builder) =
  let open Observe.Logs in
  m.untyped |+ m.field "caller" Observe.Type.string caller |> m.seal

let typed_wide_author caller (m : typed_event_builder) =
  m.typed (typed_event_patch caller)

let test_limits_defaults_and_validation () =
  let limits = Observe.Logs.Limits.default in
  Alcotest.(check int) "default depth" 64 (Observe.Logs.Limits.max_depth limits);
  Alcotest.(check int)
    "default object fields" 1_024
    (Observe.Logs.Limits.max_object_fields limits);
  Alcotest.(check int)
    "default collection length" 1_024
    (Observe.Logs.Limits.max_collection_length limits);
  Alcotest.(check int)
    "default string bytes" 1_048_576
    (Observe.Logs.Limits.max_string_bytes limits);
  Alcotest.(check int)
    "default bytes length" 1_048_576
    (Observe.Logs.Limits.max_bytes_length limits);
  Alcotest.(check int)
    "default nodes" 100_000
    (Observe.Logs.Limits.max_nodes limits);
  Alcotest.(check int)
    "default total bytes" 4_194_304
    (Observe.Logs.Limits.max_total_bytes limits);
  let check_invalid field make =
    match make () with
    | Ok _ -> Alcotest.fail "non-positive limit was accepted"
    | Error error ->
        Alcotest.(check bool)
          "invalid limit field" true
          (error.Observe.Logs.Limits.field = field);
        Alcotest.(check int)
          "invalid limit value" 0 error.Observe.Logs.Limits.value;
        Alcotest.(check bool)
          "invalid limit problem" true
          (error.Observe.Logs.Limits.problem = Observe.Logs.Limits.Non_positive)
  in
  check_invalid Observe.Logs.Limits.Max_depth (fun () ->
      Observe.Logs.Limits.create ~max_depth:0 ());
  check_invalid Observe.Logs.Limits.Max_object_fields (fun () ->
      Observe.Logs.Limits.create ~max_object_fields:0 ());
  check_invalid Observe.Logs.Limits.Max_collection_length (fun () ->
      Observe.Logs.Limits.create ~max_collection_length:0 ());
  check_invalid Observe.Logs.Limits.Max_string_bytes (fun () ->
      Observe.Logs.Limits.create ~max_string_bytes:0 ());
  check_invalid Observe.Logs.Limits.Max_bytes_length (fun () ->
      Observe.Logs.Limits.create ~max_bytes_length:0 ());
  check_invalid Observe.Logs.Limits.Max_nodes (fun () ->
      Observe.Logs.Limits.create ~max_nodes:0 ());
  check_invalid Observe.Logs.Limits.Max_total_bytes (fun () ->
      Observe.Logs.Limits.create ~max_total_bytes:0 ());
  let custom =
    Observe.Logs.Limits.create_exn ~max_depth:3 ~max_object_fields:4
      ~max_collection_length:5 ~max_string_bytes:6 ~max_bytes_length:7
      ~max_nodes:8 ~max_total_bytes:9 ()
  in
  Alcotest.(check int) "custom depth" 3 (Observe.Logs.Limits.max_depth custom);
  Alcotest.(check int)
    "custom object fields" 4
    (Observe.Logs.Limits.max_object_fields custom);
  Alcotest.(check int)
    "custom collection length" 5
    (Observe.Logs.Limits.max_collection_length custom);
  Alcotest.(check int)
    "custom string bytes" 6
    (Observe.Logs.Limits.max_string_bytes custom);
  Alcotest.(check int)
    "custom bytes length" 7
    (Observe.Logs.Limits.max_bytes_length custom);
  Alcotest.(check int) "custom nodes" 8 (Observe.Logs.Limits.max_nodes custom);
  Alcotest.(check int)
    "custom total bytes" 9
    (Observe.Logs.Limits.max_total_bytes custom);
  let calls = ref 0 in
  let lazy_enricher =
    Observe.Logs.Enricher.create_exn ~name:"lazy" (fun () ->
        incr calls;
        Observe.Value.object_ [ ("lazy", Observe.Value.bool true) ])
  in
  Alcotest.(check int) "enricher construction is lazy" 0 !calls;
  Alcotest.(check string)
    "enricher name" "lazy"
    (Observe.Logs.Enricher.name lazy_enricher);
  Alcotest.(check (list string))
    "enricher authority" []
    (Observe.Logs.Enricher.authoritative_fields lazy_enricher);
  (match
     Observe.Logs.Enricher.create ~name:"" (fun () -> Observe.Value.null)
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "empty enricher name was accepted");
  (match
     Observe.Logs.Enricher.create ~name:"reserved"
       ~authoritative_fields:[ "service" ] (fun () -> Observe.Value.null)
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "reserved authoritative field was accepted");
  let defaults = Observe.Config.create_exn ~service:"defaults" () in
  Alcotest.(check int)
    "default enrichers" 0
    (List.length (Observe.Config.enrichers defaults));
  Alcotest.(check int)
    "config default depth" 64
    (Observe.Logs.Limits.max_depth (Observe.Config.limits defaults));
  let first = make_enricher "first" [ ("first", Observe.Value.bool true) ] in
  let second = make_enricher "second" [ ("second", Observe.Value.bool true) ] in
  let configured =
    Observe.Config.create_exn ~service:"configured" ~enrichers:[ first; second ]
      ~limits:custom ()
  in
  Alcotest.(check int)
    "configured enrichers" 2
    (List.length (Observe.Config.enrichers configured));
  Alcotest.(check int)
    "configured total bytes" 9
    (Observe.Logs.Limits.max_total_bytes (Observe.Config.limits configured));
  let duplicate = make_enricher "duplicate" [] in
  (match
     Observe.Config.create ~service:"duplicate"
       ~enrichers:[ duplicate; duplicate ] ()
   with
  | Error
      {
        field = Observe.Config.Enrichers;
        problem = Observe.Config.Duplicate_enricher_name "duplicate";
      } ->
      ()
  | Error _ -> Alcotest.fail "wrong duplicate-enricher error"
  | Ok _ -> Alcotest.fail "duplicate enricher names were accepted");
  let authoritative_a =
    make_enricher ~authoritative_fields:[ "shared" ] "authority-a" []
  in
  let authoritative_b =
    make_enricher ~authoritative_fields:[ "shared" ] "authority-b" []
  in
  match
    Observe.Config.create ~service:"overlap"
      ~enrichers:[ authoritative_a; authoritative_b ]
      ()
  with
  | Error
      {
        field = Observe.Config.Enrichers;
        problem = Observe.Config.Overlapping_authoritative_field "shared";
      } ->
      ()
  | Error _ -> Alcotest.fail "wrong overlapping-authority error"
  | Ok _ -> Alcotest.fail "overlapping authoritative fields were accepted"

let test_enrichment_parity_and_outputs () =
  let calls = ref 0 in
  let enricher =
    Observe.Logs.Enricher.create_exn ~name:"context" (fun () ->
        incr calls;
        Observe.Value.object_
          [
            ("source", Observe.Value.string "enricher");
            ("shared", Observe.Value.string "enricher");
          ])
  in
  let config =
    Test_io.config ~console:Observe.Config.Silent ~enrichers:[ enricher ]
      "parity"
  in
  let capture =
    capture config (fun _ ->
        Observe.Logs.info (fun m -> m.text ~tag:"text" "%s" "hello");
        Observe.Logs.info (open_point_author "open-point");
        Observe.Logs.info (typed_point_author "typed-point");
        let wide = Observe.Logs.create ~name:"open-wide" () in
        Observe.Logs.set wide (open_wide_author "open-wide");
        Observe.Logs.emit wide;
        let typed =
          Observe.Logs.create_typed ~name:"typed-wide" ~using:typed_event_schema
            ()
        in
        Observe.Logs.set typed (typed_wide_author "typed-wide");
        Observe.Logs.emit typed)
  in
  Alcotest.(check int) "enricher invoked once per admitted observation" 5 !calls;
  let logs = Observe.Capture.logs capture in
  Alcotest.(check int) "all five authoring forms publish" 5 (List.length logs);
  List.iteri
    (fun index log ->
      check_has_field "source" log;
      check_string_field "source" "enricher" log;
      let json = json log in
      Alcotest.(check bool)
        "JSON contains enrichment" true
        (contains json "\"source\":\"enricher\"");
      let pretty = pretty log in
      Alcotest.(check bool)
        "pretty contains enrichment" true
        (String.length pretty > 0);
      match (index, Observe.Log.event log, Observe.Log.kind log) with
      | 0, Observe.Log.Text { tag; message }, Observe.Log.Point _ ->
          Alcotest.(check string) "text tag" "text" tag;
          Alcotest.(check string) "text message" "hello" message
      | ( 1,
          Observe.Log.Structured { origin = Observe.Log.Open; _ },
          Observe.Log.Point _ ) ->
          ()
      | ( 2,
          Observe.Log.Structured
            { origin = Observe.Log.Declared "enrichment_typed_event"; _ },
          Observe.Log.Point _ ) ->
          ()
      | ( 3,
          Observe.Log.Structured { origin = Observe.Log.Open; _ },
          Observe.Log.Wide _ ) ->
          ()
      | ( 4,
          Observe.Log.Structured
            { origin = Observe.Log.Declared "enrichment_typed_event"; _ },
          Observe.Log.Wide _ ) ->
          ()
      | _ -> Alcotest.failf "unexpected event form at index %d" index)
    logs

let capture_one config author =
  let capture = capture config (fun _ -> Observe.Logs.info author) in
  match Observe.Capture.logs capture with
  | [ log ] -> (log, capture)
  | logs -> Alcotest.failf "expected one log, received %d" (List.length logs)

let test_caller_wins_authority_and_order () =
  let ordinary_a = make_enricher "a" [ ("shared", Observe.Value.string "a") ] in
  let ordinary_b = make_enricher "b" [ ("shared", Observe.Value.string "b") ] in
  let caller_config enrichers =
    Test_io.config ~console:Observe.Config.Silent ~enrichers "caller-wins"
  in
  let caller_log, _ =
    capture_one
      (caller_config [ ordinary_a; ordinary_b ])
      (fun m ->
        let open Observe.Logs in
        m.untyped |+ m.field "shared" Observe.Type.string "caller" |> m.seal)
  in
  check_string_field "shared" "caller" caller_log;
  let ordered_first, _ =
    capture_one
      (caller_config [ ordinary_a; ordinary_b ])
      (fun m -> m.untyped |> m.seal)
  in
  let ordered_second, _ =
    capture_one
      (caller_config [ ordinary_b; ordinary_a ])
      (fun m -> m.untyped |> m.seal)
  in
  Alcotest.(check string)
    "ordinary conflict is order-independent"
    (json_fields ordered_first)
    (json_fields ordered_second);
  Alcotest.(check bool)
    "ordinary conflict has no arbitrary winner" true
    (not (has_field "shared" ordered_first));
  let authoritative =
    make_enricher ~authoritative_fields:[ "shared" ] "authority"
      [ ("shared", Observe.Value.string "authority") ]
  in
  let authoritative_log, _ =
    capture_one (caller_config [ authoritative ]) (fun m ->
        let open Observe.Logs in
        m.untyped |+ m.field "shared" Observe.Type.string "caller" |> m.seal)
  in
  check_string_field "shared" "authority" authoritative_log

let test_lazy_admission_and_failure_isolation () =
  let author_calls = ref 0 in
  let enricher_calls = ref 0 in
  let lazy_enricher =
    Observe.Logs.Enricher.create_exn ~name:"not-admitted" (fun () ->
        incr enricher_calls;
        Observe.Value.object_ [ ("seen", Observe.Value.bool true) ])
  in
  let rejected_config =
    Test_io.config ~console:Observe.Config.Silent ~min_level:Observe.Level.Error
      ~enrichers:[ lazy_enricher ] "rejected"
  in
  let rejected =
    capture rejected_config (fun _ ->
        Observe.Logs.info (fun m ->
            incr author_calls;
            let open Observe.Logs in
            m.untyped |> m.seal))
  in
  Alcotest.(check int) "filtered author remains lazy" 0 !author_calls;
  Alcotest.(check int) "filtered enricher remains lazy" 0 !enricher_calls;
  Alcotest.(check int)
    "filtered log is not captured" 0
    (List.length (Observe.Capture.logs rejected));
  let raising =
    Observe.Logs.Enricher.create_exn ~name:"raising" (fun () ->
        raise (Failure "enricher"))
  in
  let invalid =
    Observe.Logs.Enricher.create_exn ~name:"invalid" (fun () ->
        Observe.Value.string "not-an-object")
  in
  let malformed =
    Observe.Logs.Enricher.create_exn ~name:"malformed" (fun () ->
        Observe.Value.object_ [ ("bad", Observe.Value.string "\255") ])
  in
  let reserved =
    make_enricher "reserved"
      [
        ("service", Observe.Value.string "spoofed");
        ("kept", Observe.Value.string "yes");
      ]
  in
  let config =
    Test_io.config ~console:Observe.Config.Silent
      ~enrichers:[ raising; invalid; malformed; reserved ]
      "failures"
  in
  let log, capture =
    capture_one config (fun m ->
        let open Observe.Logs in
        m.untyped |+ m.field "caller" Observe.Type.string "preserved" |> m.seal)
  in
  check_string_field "caller" "preserved" log;
  Alcotest.(check bool)
    "reserved contribution is omitted as one unit" true
    (Option.is_none (field_value "kept" log));
  Alcotest.(check bool)
    "reserved metadata cannot be replaced" true
    (Observe.Log.service log = "failures");
  Alcotest.(check int)
    "raising enricher diagnosed once" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Enricher_raised);
  Alcotest.(check int)
    "invalid enricher diagnosed once" 2
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Enricher_invalid);
  Alcotest.(check int)
    "reserved field diagnosed once" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Enricher_reserved_field)

let open_string_author value (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped |+ m.field "v" Observe.Type.string value |> m.seal

let open_bytes_author value (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped |+ m.field "v" Observe.Type.bytes value |> m.seal

let open_list_author value (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped |+ m.field "value" Observe.Type.(list int) value |> m.seal

let open_nested_author depth (m : Observe.Logs.builder) =
  let rec nested depth (m : Observe.Logs.untyped_builder) =
    if depth = 0 then m.seal m.untyped
    else
      let open Observe.Logs in
      m.untyped
      |+ m.object_ "nested" (fun child -> nested (depth - 1) child)
      |> m.seal
  in
  let root =
    if depth = 0 then m.untyped
    else
      let open Observe.Logs in
      m.untyped |+ m.object_ "nested" (fun child -> nested (depth - 1) child)
  in
  m.seal root

let limits_with ?(max_depth = 100) ?(max_object_fields = 100)
    ?(max_collection_length = 100) ?(max_string_bytes = 10_000)
    ?(max_bytes_length = 10_000) ?(max_nodes = 10_000)
    ?(max_total_bytes = 1_000_000) () =
  Observe.Logs.Limits.create_exn ~max_depth ~max_object_fields
    ~max_collection_length ~max_string_bytes ~max_bytes_length ~max_nodes
    ~max_total_bytes ()

let bounded_log limits author =
  capture_one
    (Test_io.config ~console:Observe.Config.Silent ~limits "svc")
    author

let test_localized_truncation_boundaries () =
  let below, _ =
    bounded_log (limits_with ~max_string_bytes:4 ()) (open_string_author "abc")
  in
  check_string_field "v" "abc" below;
  let at, _ =
    bounded_log (limits_with ~max_string_bytes:4 ()) (open_string_author "abcd")
  in
  check_string_field "v" "abcd" at;
  let above, _ =
    bounded_log
      (limits_with ~max_string_bytes:4 ())
      (open_string_author "abcde")
  in
  check_truncation "v" Observe.Value.String_bytes above;
  let bytes_above, _ =
    bounded_log
      (limits_with ~max_bytes_length:2 ())
      (open_bytes_author (Bytes.of_string "abc"))
  in
  check_truncation "v" Observe.Value.Bytes_length bytes_above;
  let collection_above, _ =
    bounded_log
      (limits_with ~max_collection_length:2 ())
      (open_list_author [ 1; 2; 3 ])
  in
  (match field_value "value" collection_above with
  | Some value -> (
      match Observe.Value.view value with
      | `Truncated_list (values, Observe.Value.Collection) ->
          Alcotest.(check int)
            "collection keeps safe prefix" 2 (List.length values)
      | `List [ _; _; marker ] -> (
          match Observe.Value.view marker with
          | `Truncated Observe.Value.Collection -> ()
          | _ -> Alcotest.fail "collection marker has the wrong identity")
      | _ -> Alcotest.fail "collection was not localized at its tail")
  | None -> Alcotest.fail "missing collection field");
  let object_above, _ =
    bounded_log (limits_with ~max_object_fields:2 ()) (fun m ->
        let open Observe.Logs in
        let fields =
          m.untyped
          |+ m.field "first" Observe.Type.int 1
          |+ m.field "second" Observe.Type.int 2
          |+ m.field "third" Observe.Type.int 3
        in
        m.seal fields)
  in
  let object_values, object_is_truncated =
    match Observe.Value.view (Observe.Log.fields object_above) with
    | `Object fields -> (fields, false)
    | `Truncated_object (fields, Observe.Value.Object_fields) -> (fields, true)
    | _ -> Alcotest.fail "object truncation lost its safe prefix"
  in
  Alcotest.(check bool)
    "object keeps safe prefix" true
    (List.exists (fun (name, _) -> name = "first") object_values);
  Alcotest.(check bool) "object has a localized marker" true object_is_truncated;
  let depth_above, _ =
    bounded_log (limits_with ~max_depth:1 ()) (open_nested_author 3)
  in
  let rec contains_depth_marker value =
    match Observe.Value.view value with
    | `Truncated Observe.Value.Depth -> true
    | `Truncated _ -> false
    | `Truncated_list (values, _) -> List.exists contains_depth_marker values
    | `Truncated_object (fields, _) ->
        List.exists (fun (_, value) -> contains_depth_marker value) fields
    | `Object fields ->
        List.exists (fun (_, value) -> contains_depth_marker value) fields
    | `List values -> List.exists contains_depth_marker values
    | `Variant (_, _, Some value) -> contains_depth_marker value
    | `Variant (_, _, None)
    | `Null | `Bool _ | `Integer _ | `Float _ | `String _ | `Bytes _ ->
        false
  in
  Alcotest.(check bool)
    "depth has a localized marker" true
    (contains_depth_marker (Observe.Log.fields depth_above));
  let caller_literal, _ =
    bounded_log
      (limits_with ~max_string_bytes:100 ())
      (open_string_author "<truncated:string>")
  in
  (match field_value "v" caller_literal with
  | Some value -> (
      match Observe.Value.view value with
      | `String literal ->
          Alcotest.(check string)
            "caller marker-looking scalar" "<truncated:string>" literal
      | `Truncated _ | `Truncated_list _ | `Truncated_object _ ->
          Alcotest.fail "caller scalar became a package marker"
      | _ -> Alcotest.fail "unexpected caller scalar shape")
  | None -> Alcotest.fail "missing caller scalar");
  let rendered = json above in
  let displayed = pretty above in
  Alcotest.(check bool)
    "JSON exposes stable marker text" true
    (String.contains rendered '<');
  Alcotest.(check bool)
    "pretty exposes stable marker text" true
    (String.contains displayed '<')

let test_aggregate_enrichment_preserves_safe_contributions () =
  let first = make_enricher "a" [ ("a", Observe.Value.string "a") ] in
  let second = make_enricher "b" [ ("b", Observe.Value.string "b") ] in
  let limits = limits_with ~max_total_bytes:250 () in
  let log, _ =
    capture_one
      (Test_io.config ~console:Observe.Config.Silent
         ~enrichers:[ second; first ] ~limits "svc")
      (fun m -> m.seal m.untyped)
  in
  match Observe.Value.view (Observe.Log.fields log) with
  | `Truncated_object (fields, Observe.Value.Total_bytes) ->
      let first = List.assoc_opt "a" fields in
      let second = List.assoc_opt "b" fields in
      (match first with
      | Some value -> (
          match Observe.Value.view value with
          | `String value -> Alcotest.(check string) "safe enrichment" "a" value
          | _ -> Alcotest.fail "safe enrichment changed shape")
      | None -> Alcotest.fail "safe enrichment was omitted");
      Alcotest.(check bool)
        "over-budget enrichment is omitted" true (Option.is_none second)
  | _ -> Alcotest.fail "aggregate enrichment limit had no package marker"

let () =
  Alcotest.run "observe-enrichment"
    [
      ( "contracts",
        [
          Alcotest.test_case "limits, config, and enricher validation" `Quick
            test_limits_defaults_and_validation;
          Alcotest.test_case "enrichment parity and outputs" `Quick
            test_enrichment_parity_and_outputs;
          Alcotest.test_case "caller wins, authority, and order" `Quick
            test_caller_wins_authority_and_order;
          Alcotest.test_case "lazy admission and failure isolation" `Quick
            test_lazy_admission_and_failure_isolation;
          Alcotest.test_case "localized truncation boundaries" `Quick
            test_localized_truncation_boundaries;
          Alcotest.test_case "aggregate enrichment preserves safe context"
            `Quick test_aggregate_enrichment_preserves_safe_contributions;
        ] );
    ]
