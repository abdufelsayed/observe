module Observer = Observe.Make (Test_io.IO)
module Redaction = Observe.Logs.Redaction

let observer = Observer.create (Test_io.Host.create ())

let capture ?(min_level = Observe.Level.Debug) ?redaction ?limits
    ?(service = "redaction") ?version callback =
  let config =
    Observe.Config.create_exn ~service ~environment:"test" ?version
      ~console:Observe.Config.Silent ~min_level ?redaction ?limits ()
  in
  match
    Observer.with_capture observer ~config (fun capture ->
        callback capture;
        capture)
  with
  | Ok value -> value
  | Error Observe.IO_already_registered ->
      Alcotest.fail "redaction capture route unexpectedly conflicted"
  | Error (Observe.Invalid_capacity capacity) ->
      Alcotest.failf "unexpected invalid capacity: %d" capacity
  | Error Observe.Runtime_closed -> Alcotest.fail "redaction runtime was closed"

let one_log capture =
  match Observe.Capture.logs capture with
  | [ log ] -> log
  | logs -> Alcotest.failf "expected one log, received %d" (List.length logs)

let logs capture = Observe.Capture.logs capture

let json_fields log =
  Observe.Value.frozen_to_json_string (Observe.Log.fields log)

let object_fields log =
  match Observe.Value.view (Observe.Log.fields log) with
  | `Object fields -> fields
  | _ -> Alcotest.fail "expected an object field root"

let field_value name log =
  List.find_map
    (fun (candidate, value) ->
      if String.equal name candidate then Some value else None)
    (object_fields log)

let has_field name log = Option.is_some (field_value name log)

let string_value value =
  match Observe.Value.view value with
  | `String value -> value
  | _ -> Alcotest.fail "expected a string value"

let nested_object_field object_name field_name log =
  let fields =
    match field_value object_name log with
    | Some value -> (
        match Observe.Value.view value with
        | `Object fields -> fields
        | _ -> Alcotest.failf "field %s was not an object" object_name)
    | None ->
        Alcotest.failf "missing field %s in %s" object_name (json_fields log)
  in
  List.find_map
    (fun (candidate, value) ->
      if String.equal candidate field_name then Some value else None)
    fields

let expect_nested_string object_name field_name expected log =
  match nested_object_field object_name field_name log with
  | Some value ->
      Alcotest.(check string)
        (object_name ^ "." ^ field_name)
        expected (string_value value)
  | None ->
      Alcotest.failf "missing field %s.%s in %s" object_name field_name
        (json_fields log)

let expect_no_nested_field object_name field_name log =
  Alcotest.(check bool)
    (object_name ^ "." ^ field_name ^ " is absent")
    false
    (Option.is_some (nested_object_field object_name field_name log))

let string_field name log =
  match field_value name log with
  | Some value -> string_value value
  | None -> Alcotest.failf "missing field %s in %s" name (json_fields log)

let expect_string_field name expected log =
  if not (has_field name log) then
    Alcotest.failf "missing field %s in %s" name (json_fields log);
  Alcotest.(check string) ("field " ^ name) expected (string_field name log)

let expect_no_field name log =
  Alcotest.(check bool)
    ("field " ^ name ^ " is absent")
    false (has_field name log)

let list_strings name log =
  match field_value name log with
  | Some value -> (
      match Observe.Value.view value with
      | `List values -> List.map string_value values
      | _ -> Alcotest.failf "field %s was not a list" name)
  | None -> Alcotest.failf "missing field %s in %s" name (json_fields log)

let variant_string name log =
  match field_value name log with
  | Some value -> (
      match Observe.Value.view value with
      | `Variant ("Rejected", _, Some payload) -> string_value payload
      | `Variant (actual, _, _) ->
          Alcotest.failf "field %s had variant %s" name actual
      | _ -> Alcotest.failf "field %s was not a variant" name)
  | None -> Alcotest.failf "missing field %s in %s" name (json_fields log)

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search offset =
    if offset + fragment_length > text_length then false
    else if String.sub text offset fragment_length = fragment then true
    else search (offset + 1)
  in
  fragment_length = 0 || search 0

let format formatter log =
  match Observe.Formatter.format formatter log with
  | Ok output -> output
  | Error _ -> Alcotest.fail "formatter rejected a redacted log"

let diagnostic_count capture kind =
  List.find_map
    (fun (entry : Observe.Diagnostics.entry) ->
      if entry.kind = kind then Some entry.count else None)
    (Observe.Capture.diagnostics capture)
  |> Option.value ~default:0

type phase = Started | Rejected of string

let phase_t =
  let open Observe.Type in
  variant "redaction_phase" (fun started rejected -> function
    | Started -> started
    | Rejected reason -> rejected reason)
  |~ case0
       ~is:(function Started -> true | Rejected _ -> false)
       "Started" Started
  |~ case1
       ~project:(function Rejected reason -> Some reason | Started -> None)
       "Rejected" string
       (fun reason -> Rejected reason)
  |> sealv

let open_nested_author card items phase (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped
  |+ m.field "token" Observe.Type.string "sk_live_top"
  |+ m.object_ "payment" (fun payment ->
      payment.untyped
      |+ payment.field "card_number" Observe.Type.string card
      |+ payment.field "cvv" Observe.Type.string "123"
      |> payment.seal)
  |+ m.field "items" (Observe.Type.list Observe.Type.string) items
  |+ m.field "phase" phase_t phase
  |> m.seal

let path fields = Redaction.Path.fields fields

let redaction_effects log =
  List.map Observe.Log.redaction_effect (Observe.Log.redactions log)

let redaction_locations log =
  List.map Observe.Log.redaction_location (Observe.Log.redactions log)

let has_structured_location expected log =
  List.exists
    (function
      | Observe.Log.Structured_value actual -> String.equal expected actual
      | Observe.Log.Text_message | Observe.Log.Annotation_message _ -> false)
    (redaction_locations log)

let has_annotation_location expected log =
  List.exists
    (function
      | Observe.Log.Annotation_message actual -> actual = expected
      | Observe.Log.Structured_value _ | Observe.Log.Text_message -> false)
    (redaction_locations log)

let has_text_location log =
  List.exists
    (function
      | Observe.Log.Text_message -> true
      | Observe.Log.Structured_value _ | Observe.Log.Annotation_message _ ->
          false)
    (redaction_locations log)

let has_effect expected log =
  List.exists (fun actual -> actual = expected) (redaction_effects log)

let test_default_policy_is_inert () =
  let source = ref "sk_live_default" in
  let log =
    one_log
      (capture (fun _ ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped
               |+ m.field "token" Observe.Type.string !source
               |> m.seal)))
  in
  source := "changed-after-authorship";
  expect_string_field "token" "sk_live_default" log;
  Alcotest.(check int)
    "omitted policy has no evidence" 0
    (List.length (Observe.Log.redactions log));
  let defaults = Observe.Config.create_exn ~service:"defaults" () in
  Alcotest.(check bool)
    "omitted policy does not guess" true
    (Redaction.is_none (Observe.Config.redaction defaults))

let test_exact_nested_list_case_and_absent_paths () =
  let finite =
    Redaction.Mask.keep_suffix ~characters:4 ~hidden:(Redaction.Mask.Fill "*")
      ()
  in
  let policy =
    Redaction.create_exn
      ~rules:
        [
          Redaction.Rule.at (path [ "payment"; "cvv" ]) Redaction.Action.remove;
          Redaction.Rule.at
            (path [ "payment"; "card_number" ])
            (Redaction.Action.mask finite);
          Redaction.Rule.at
            (Redaction.Path.root
            |> Redaction.Path.field "items"
            |> Redaction.Path.index 1)
            (Redaction.Action.replace (Observe.Value.string "[item]"));
          Redaction.Rule.at
            (Redaction.Path.root
            |> Redaction.Path.field "phase"
            |> Redaction.Path.case "Rejected")
            (Redaction.Action.replace (Observe.Value.string "[reason]"));
          Redaction.Rule.at (path [ "does_not_exist" ]) Redaction.Action.remove;
        ]
      ()
  in
  let log =
    one_log
      (capture ~redaction:policy (fun _ ->
           Observe.Logs.info
             (open_nested_author "π😀1234" [ "first"; "second" ]
                (Rejected "card declined"))))
  in
  expect_string_field "token" "sk_live_top" log;
  expect_no_nested_field "payment" "cvv" log;
  expect_nested_string "payment" "card_number" "**1234" log;
  Alcotest.(check (list string))
    "exact list index is replaced" [ "first"; "[item]" ]
    (list_strings "items" log);
  Alcotest.(check string)
    "exact variant payload is replaced" "[reason]"
    (variant_string "phase" log);
  expect_no_field "does_not_exist" log;
  Alcotest.(check bool)
    "remove was reported" true
    (has_effect Observe.Log.Removed log);
  Alcotest.(check bool)
    "mask path was reported" true
    (has_structured_location "$[\"payment\"][\"card_number\"]" log)

let test_repeated_wide_contributions () =
  let finite =
    Redaction.Mask.keep_suffix ~characters:4 ~hidden:(Redaction.Mask.Fill "*")
      ()
  in
  let policy =
    Redaction.create_exn
      ~rules:
        [
          Redaction.Rule.at (path [ "payment"; "cvv" ]) Redaction.Action.remove;
          Redaction.Rule.at
            (path [ "payment"; "card_number" ])
            (Redaction.Action.mask finite);
        ]
      ()
  in
  let captured =
    capture ~redaction:policy (fun _ ->
        let wide = Observe.Logs.create ~name:"repeated" () in
        Observe.Logs.set wide (fun m ->
            let open Observe.Logs in
            m.untyped
            |+ m.object_ "payment" (fun payment ->
                payment.untyped
                |+ payment.field "card_number" Observe.Type.string "π😀1234"
                |> payment.seal)
            |> m.seal);
        Observe.Logs.set wide (fun m ->
            let open Observe.Logs in
            m.untyped
            |+ m.object_ "payment" (fun payment ->
                payment.untyped
                |+ payment.field "cvv" Observe.Type.string "123"
                |> payment.seal)
            |> m.seal);
        Observe.Logs.emit wide)
  in
  let log = one_log captured in
  expect_no_nested_field "payment" "cvv" log;
  expect_nested_string "payment" "card_number" "**1234" log

let test_custom_masks_and_failure_fallback () =
  let seen = ref None in
  let custom =
    Redaction.Mask.custom ~fallback:"[CUSTOM-FALLBACK]" (fun value ->
        seen := Some value;
        "masked:" ^ value)
  in
  let success_policy =
    Redaction.create_exn
      ~rules:
        [ Redaction.Rule.at (path [ "email" ]) (Redaction.Action.mask custom) ]
      ()
  in
  let success =
    one_log
      (capture ~redaction:success_policy (fun _ ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped
               |+ m.field "email" Observe.Type.string "person@example.com"
               |> m.seal)))
  in
  expect_string_field "email" "masked:person@example.com" success;
  Alcotest.(check (option string))
    "custom receives the matched value" (Some "person@example.com") !seen;
  let failing =
    Redaction.Mask.custom ~fallback:"[CUSTOM-FALLBACK]" (fun _ ->
        raise (Failure "mask callback"))
  in
  let failure_policy =
    Redaction.create_exn
      ~rules:
        [ Redaction.Rule.at (path [ "email" ]) (Redaction.Action.mask failing) ]
      ()
  in
  let failure =
    one_log
      (capture ~redaction:failure_policy (fun _ ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped
               |+ m.field "email" Observe.Type.string "person@example.com"
               |> m.seal)))
  in
  expect_string_field "email" "[CUSTOM-FALLBACK]" failure;
  Alcotest.(check bool)
    "failed custom mask is reported" true
    (has_effect Observe.Log.Failed_closed failure)

let test_matching_structured_text_and_annotation () =
  let policy =
    Redaction.create_exn
      ~rules:
        [
          Redaction.Rule.matching
            (Redaction.Matcher.string_prefix "sk_live_")
            (Redaction.Action.replace (Observe.Value.string "[secret]"));
        ]
      ()
  in
  let source = ref "sk_live_structured" in
  let config_service = "sk_live_service" in
  let captured =
    capture ~service:config_service ~version:"sk_live_version" ~redaction:policy
      (fun _ ->
        Observe.Logs.info (fun m ->
            let open Observe.Logs in
            m.untyped |+ m.field "token" Observe.Type.string !source |> m.seal);
        Observe.Logs.info (fun m ->
            m.text ~tag:"sk_live_tag" "%s" "sk_live_point_message");
        let wide = Observe.Logs.create ~name:"sk_live_operation" () in
        Observe.Logs.annotate wide ~level:Observe.Level.Info (fun () ->
            "sk_live_annotation");
        Observe.Logs.emit wide)
  in
  source := "changed-after-authorship";
  match logs captured with
  | [ structured; text; wide ] ->
      expect_string_field "token" "[secret]" structured;
      (match Observe.Log.event text with
      | Observe.Log.Text { tag; message } ->
          Alcotest.(check string) "point text tag is metadata" "sk_live_tag" tag;
          Alcotest.(check string) "point text is matched" "[secret]" message
      | Observe.Log.Structured _ -> Alcotest.fail "expected text point");
      (match Observe.Log.kind wide with
      | Observe.Log.Wide { operation; annotations } ->
          Alcotest.(check string)
            "operation name is metadata" "sk_live_operation"
            (Observe.Log.operation_name operation);
          (match annotations with
          | [ annotation ] ->
              Alcotest.(check string)
                "annotation message is matched" "[secret]"
                (Observe.Log.annotation_message annotation)
          | _ -> Alcotest.fail "expected one annotation");
          Alcotest.(check bool)
            "annotation report points at its index" true
            (has_annotation_location 0 wide)
      | Observe.Log.Point _ -> Alcotest.fail "expected wide log");
      Alcotest.(check string)
        "service metadata is untouched" config_service
        (Observe.Log.service structured);
      Alcotest.(check (option string))
        "environment metadata is untouched" (Some "test")
        (Observe.Log.environment structured);
      Alcotest.(check (option string))
        "version metadata is untouched" (Some "sk_live_version")
        (Observe.Log.version structured);
      Alcotest.(check bool)
        "text redaction is separately reported" true (has_text_location text);
      Alcotest.(check bool)
        "JSON has no redaction marker" false
        (contains (json_fields structured) "redaction");
      Alcotest.(check bool)
        "all matched regions are reported" true
        (List.length (Observe.Log.redactions structured) = 1
        && List.length (Observe.Log.redactions text) = 1
        && List.length (Observe.Log.redactions wide) = 1)
  | actual ->
      Alcotest.failf "expected structured, text, and wide logs, received %d"
        (List.length actual)

type typed_event = { token : string }

let typed_event_t =
  let open Observe.Type in
  record "same_name" (fun token -> { token })
  |+ field "token" string (fun event -> event.token)
  |> sealr

type typed_targets = { count : int; phase : phase }

let typed_targets_t =
  let open Observe.Type in
  record "typed_targets" (fun count phase -> { count; phase })
  |+ field "count" int (fun value -> value.count)
  |+ field "phase" phase_t (fun value -> value.phase)
  |> sealr

type table_target = { entries : (string, string) Hashtbl.t }

let table_target_t =
  let open Observe.Type in
  record "table_target" (fun entries -> { entries })
  |+ field "entries" (hashtbl string string) (fun value -> value.entries)
  |> sealr

type typed_event_builder = {
  token : string -> typed_event Observe.Schema.patch;
}

let make_typed_schema ?(description = typed_event_t) () =
  let patch_builder : typed_event Observe.Schema.patch_builder option ref =
    ref None
  in
  let schema =
    Observe.Schema.record description ~builder:(fun builder ->
        patch_builder := Some builder;
        { token = Observe.Schema.field builder "token" Observe.Type.string })
  in
  let patch value =
    match !patch_builder with
    | Some builder ->
        Observe.Schema.field builder "token" Observe.Type.string value
    | None -> Alcotest.fail "typed schema builder was not initialized"
  in
  (schema, patch)

let test_typed_schema_identity_and_forms () =
  let schema_a, _patch_a = make_typed_schema () in
  let schema_b, _patch_b = make_typed_schema () in
  let policy =
    Redaction.create_exn ~using:schema_a
      ~rules:[ Redaction.Rule.at (path [ "token" ]) Redaction.Action.remove ]
      ()
  in
  let captured =
    capture ~redaction:policy (fun _ ->
        Observe.Logs.info (fun m ->
            m.typed ~using:schema_a { token = "typed-a" });
        Observe.Logs.info (fun m ->
            m.typed ~using:schema_b { token = "typed-b" }))
  in
  match logs captured with
  | [ same_schema; distinct_schema ] -> (
      expect_no_field "token" same_schema;
      expect_string_field "token" "typed-b" distinct_schema;
      match Observe.Log.event same_schema with
      | Observe.Log.Structured { origin = Observe.Log.Declared "same_name" } ->
          ()
      | _ -> Alcotest.fail "expected declared typed point")
  | actual ->
      Alcotest.failf "expected two typed points, received %d"
        (List.length actual)

let test_opaque_typed_schema_defers_path_validation () =
  let mapped = Observe.Type.map typed_event_t Fun.id Fun.id in
  let schema, _patch = make_typed_schema ~description:mapped () in
  let policy =
    Redaction.create_exn ~using:schema
      ~rules:[ Redaction.Rule.at (path [ "token" ]) Redaction.Action.remove ]
      ()
  in
  let log =
    one_log
      (capture ~redaction:policy (fun _ ->
           Observe.Logs.info (fun m ->
               m.typed ~using:schema { token = "opaque-secret" })))
  in
  expect_no_field "token" log

let test_open_and_typed_wide_forms () =
  let policy =
    Redaction.create_exn
      ~rules:
        [
          Redaction.Rule.matching
            (Redaction.Matcher.string_prefix "secret-")
            (Redaction.Action.replace (Observe.Value.string "[safe]"));
        ]
      ()
  in
  let schema, _patch = make_typed_schema () in
  let captured =
    capture ~redaction:policy (fun _ ->
        let open_wide = Observe.Logs.create ~name:"open-wide" () in
        Observe.Logs.set open_wide (fun m ->
            let open Observe.Logs in
            m.untyped
            |+ m.field "token" Observe.Type.string "secret-open"
            |> m.seal);
        Observe.Logs.emit open_wide;
        let typed_wide =
          Observe.Logs.create_typed ~name:"typed-wide" ~using:schema ()
        in
        Observe.Logs.set typed_wide (fun m -> m.token "secret-typed");
        Observe.Logs.emit typed_wide)
  in
  match logs captured with
  | [ open_wide; typed_wide ] -> (
      expect_string_field "token" "[safe]" open_wide;
      expect_string_field "token" "[safe]" typed_wide;
      (match Observe.Log.event open_wide with
      | Observe.Log.Structured { origin = Observe.Log.Open } -> ()
      | _ -> Alcotest.fail "expected open wide event");
      match Observe.Log.event typed_wide with
      | Observe.Log.Structured { origin = Observe.Log.Declared "same_name" } ->
          ()
      | _ -> Alcotest.fail "expected typed wide event")
  | actual ->
      Alcotest.failf "expected two wide logs, received %d" (List.length actual)

let test_replacement_rechecked_and_rule_composition () =
  let token_path = path [ "token" ] in
  let matcher =
    Redaction.Rule.matching
      (Redaction.Matcher.string_prefix "sk_live_")
      (Redaction.Action.replace (Observe.Value.string "[global]"))
  in
  let exact =
    Redaction.Rule.at token_path
      (Redaction.Action.replace (Observe.Value.string "sk_live_replacement"))
  in
  let policy = Redaction.create_exn ~rules:[ exact; matcher ] () in
  let log =
    one_log
      (capture ~redaction:policy (fun _ ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped
               |+ m.field "token" Observe.Type.string "original"
               |> m.seal)))
  in
  expect_string_field "token" "[global]" log;
  let duplicate = Redaction.create_exn ~rules:[ exact; exact ] () in
  let duplicate_log =
    one_log
      (capture ~redaction:duplicate (fun _ ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped
               |+ m.field "token" Observe.Type.string "original"
               |> m.seal)))
  in
  expect_string_field "token" "sk_live_replacement" duplicate_log;
  Alcotest.(check int)
    "identical rules are deduplicated" 1
    (List.length (Observe.Log.redactions duplicate_log));
  let ancestor =
    Redaction.create_exn
      ~rules:
        [
          Redaction.Rule.at (path [ "payment" ]) Redaction.Action.remove;
          Redaction.Rule.at
            (path [ "payment"; "cvv" ])
            (Redaction.Action.replace (Observe.Value.string "ignored"));
        ]
      ()
  in
  let ancestor_log =
    one_log
      (capture ~redaction:ancestor (fun _ ->
           Observe.Logs.info
             (open_nested_author "card" [ "item" ] (Rejected "reason"))))
  in
  expect_no_field "payment" ancestor_log;
  (match
     Redaction.create
       ~rules:
         [
           Redaction.Rule.at token_path Redaction.Action.remove;
           Redaction.Rule.at token_path
             (Redaction.Action.replace (Observe.Value.string "other"));
         ]
       ()
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "conflicting exact actions were accepted");
  let ordered_left =
    Redaction.create_exn
      ~rules:
        [
          Redaction.Rule.at (path [ "payment"; "cvv" ]) Redaction.Action.remove;
          Redaction.Rule.at
            (Redaction.Path.root
            |> Redaction.Path.field "items"
            |> Redaction.Path.index 1)
            (Redaction.Action.replace (Observe.Value.string "[item]"));
        ]
      ()
  in
  let ordered_right =
    Redaction.create_exn
      ~rules:
        [
          Redaction.Rule.at
            (Redaction.Path.root
            |> Redaction.Path.field "items"
            |> Redaction.Path.index 1)
            (Redaction.Action.replace (Observe.Value.string "[item]"));
          Redaction.Rule.at (path [ "payment"; "cvv" ]) Redaction.Action.remove;
        ]
      ()
  in
  let left =
    one_log
      (capture ~redaction:ordered_left (fun _ ->
           Observe.Logs.info (open_nested_author "card" [ "a"; "b" ] Started)))
  in
  let right =
    one_log
      (capture ~redaction:ordered_right (fun _ ->
           Observe.Logs.info (open_nested_author "card" [ "a"; "b" ] Started)))
  in
  Alcotest.(check string)
    "rule order is not semantic" (json_fields left) (json_fields right)

let test_lazy_authoring () =
  let policy =
    Redaction.create_exn
      ~rules:
        [
          Redaction.Rule.matching
            (Redaction.Matcher.string_prefix "secret-")
            (Redaction.Action.replace (Observe.Value.string "[safe]"));
        ]
      ()
  in
  let calls = ref 0 in
  let captured =
    capture ~min_level:Observe.Level.Info ~redaction:policy (fun capture ->
        Observe.Logs.debug (fun m ->
            incr calls;
            let open Observe.Logs in
            m.untyped
            |+ m.field "token" Observe.Type.string "secret-debug"
            |> m.seal);
        capture)
  in
  Alcotest.(check int) "filtered author remains lazy" 0 !calls;
  Alcotest.(check int)
    "filtered log is not captured" 0
    (List.length (logs captured))

let test_stricter_drain_only_reduces_disclosure () =
  let global =
    Redaction.create_exn
      ~rules:
        [
          Redaction.Rule.matching
            (Redaction.Matcher.string_prefix "secret-")
            (Redaction.Action.replace (Observe.Value.string "[global]"));
        ]
      ()
  in
  let branch =
    Redaction.create_exn
      ~rules:[ Redaction.Rule.at (path [ "token" ]) Redaction.Action.remove ]
      ()
  in
  let received = ref [] in
  let drain =
    Observe.Drain.create (fun log ->
        received := log :: !received;
        Observe.Drain.Accepted)
    |> Observe.Drain.with_redaction ~redaction:branch
  in
  let config =
    Observe.Config.create_exn ~service:"drain" ~environment:"test"
      ~console:Observe.Config.Silent ~redaction:global ~drains:[ drain ] ()
  in
  Observer.init_exn observer config;
  Observe.Logs.info (fun m ->
      let open Observe.Logs in
      m.untyped |+ m.field "token" Observe.Type.string "secret-token" |> m.seal);
  Observer.close observer;
  match !received with
  | [ log ] ->
      expect_no_field "token" log;
      Alcotest.(check bool)
        "branch never sees a more revealing value" true
        (not (contains (json_fields log) "secret-token"))
  | actual ->
      Alcotest.failf "expected one drained log, received %d"
        (List.length actual)

let test_dynamic_conflict_fails_closed_and_formats_safely () =
  let policy =
    Redaction.create_exn
      ~rules:
        [
          Redaction.Rule.matching
            (Redaction.Matcher.string_prefix "secret-")
            (Redaction.Action.replace (Observe.Value.string "[prefix]"));
          Redaction.Rule.matching
            (Redaction.Matcher.string_suffix "-value")
            (Redaction.Action.replace (Observe.Value.string "[suffix]"));
        ]
      ()
  in
  let captured =
    capture ~redaction:policy (fun _ ->
        Observe.Logs.info (fun m ->
            let open Observe.Logs in
            m.untyped
            |+ m.field "secret" Observe.Type.string "secret-source-value"
            |+ m.field "safe" Observe.Type.string "retained"
            |> m.seal))
  in
  let log = one_log captured in
  expect_no_field "secret" log;
  expect_string_field "safe" "retained" log;
  Alcotest.(check bool)
    "dynamic conflict is reported on the affected value" true
    (has_effect Observe.Log.Failed_closed log);
  Alcotest.(check int)
    "dynamic conflict records one bounded diagnostic" 1
    (diagnostic_count captured Observe.Diagnostics.Redaction_conflict);
  let json = format Observe.Formatter.json log in
  let pretty = format (Observe.Formatter.pretty Observe.Formatter.Plain) log in
  List.iter
    (fun output ->
      Alcotest.(check bool)
        "formatted output retains the safe sibling" true
        (contains output "retained");
      Alcotest.(check bool)
        "formatted output never exposes the conflicting source" false
        (contains output "secret-source-value");
      Alcotest.(check bool)
        "formatted output has no redaction wrapper" false
        (contains output "_observe_redacted" || contains output "\"redactions\""))
    [ json; pretty ]

let test_overlapping_contains_are_complete_and_order_independent () =
  let matching pattern replacement =
    Redaction.Rule.matching
      (Redaction.Matcher.string_contains pattern)
      (Redaction.Action.replace (Observe.Value.string replacement))
  in
  let emit policy =
    one_log
      (capture ~redaction:policy (fun _ ->
           Observe.Logs.info (fun m ->
               let open Observe.Logs in
               m.untyped
               |+ m.field "secret" Observe.Type.string "she"
               |+ m.field "safe" Observe.Type.string "retained"
               |> m.seal)))
  in
  let same_action =
    Redaction.create_exn
      ~rules:[ matching "he" "[safe]"; matching "she" "[safe]" ]
      ()
  in
  expect_string_field "secret" "[safe]" (emit same_action);
  let conflicting = [ matching "he" "[one]"; matching "she" "[two]" ] in
  List.iter
    (fun rules ->
      let log = emit (Redaction.create_exn ~rules ()) in
      expect_no_field "secret" log;
      expect_string_field "safe" "retained" log;
      Alcotest.(check bool)
        "overlapping contains actions fail closed" true
        (has_effect Observe.Log.Failed_closed log))
    [ conflicting; List.rev conflicting ]

let test_custom_invalid_and_over_budget_results_use_fallback () =
  let limits =
    Observe.Logs.Limits.create_exn ~max_string_bytes:32 ~max_total_bytes:16_384
      ()
  in
  let custom =
    Redaction.Mask.custom ~fallback:"[safe]" (function
      | "invalid" -> "\255"
      | _ -> String.make 33 'x')
  in
  let policy =
    Redaction.create_exn
      ~rules:
        [ Redaction.Rule.at (path [ "value" ]) (Redaction.Action.mask custom) ]
      ()
  in
  let captured =
    capture ~limits ~redaction:policy (fun _ ->
        List.iter
          (fun value ->
            Observe.Logs.info (fun m ->
                let open Observe.Logs in
                m.untyped |+ m.field "value" Observe.Type.string value |> m.seal))
          [ "invalid"; "too-large" ])
  in
  match logs captured with
  | [ invalid; over_budget ] ->
      expect_string_field "value" "[safe]" invalid;
      expect_string_field "value" "[safe]" over_budget;
      Alcotest.(check bool)
        "invalid custom result is failed closed" true
        (has_effect Observe.Log.Failed_closed invalid);
      Alcotest.(check bool)
        "over-budget custom result is failed closed" true
        (has_effect Observe.Log.Failed_closed over_budget)
  | actual ->
      Alcotest.failf "expected two custom-mask logs, received %d"
        (List.length actual)

let test_policy_validation_rejects_impossible_actions () =
  let mask =
    Redaction.Mask.keep_suffix ~characters:2 ~hidden:(Redaction.Mask.Fill "*")
      ()
  in
  let expect_policy_error name rule =
    match Redaction.create ~rules:[ rule ] () with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "%s unexpectedly produced a policy" name
  in
  let expect_typed_policy_error : type record builder.
      string -> (record, builder) Observe.Schema.t -> Redaction.Rule.t -> unit =
   fun name using rule ->
    match Redaction.create ~using ~rules:[ rule ] () with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "%s unexpectedly produced a typed policy" name
  in
  expect_policy_error "root mask"
    (Redaction.Rule.at Redaction.Path.root (Redaction.Action.mask mask));
  expect_policy_error "non-object root replacement"
    (Redaction.Rule.at Redaction.Path.root
       (Redaction.Action.replace (Observe.Value.string "replacement")));
  expect_policy_error "non-string text replacement"
    (Redaction.Rule.matching
       (Redaction.Matcher.string_equal "secret")
       (Redaction.Action.replace (Observe.Value.int 0)));
  let max_string_bytes =
    Observe.Logs.Limits.max_string_bytes Observe.Logs.Limits.default
  in
  let oversized = String.make (max_string_bytes + 1) 'x' in
  expect_policy_error "truncated replacement"
    (Redaction.Rule.at (path [ "token" ])
       (Redaction.Action.replace (Observe.Value.string oversized)));
  expect_policy_error "non-string matching mask"
    (Redaction.Rule.matching (Redaction.Matcher.int 42)
       (Redaction.Action.mask mask));
  expect_policy_error "matching removal"
    (Redaction.Rule.matching
       (Redaction.Matcher.string_equal "secret")
       Redaction.Action.remove);
  let typed_targets_schema =
    Observe.Schema.record typed_targets_t ~builder:(fun _ -> ())
  in
  expect_typed_policy_error "integer field mask" typed_targets_schema
    (Redaction.Rule.at (path [ "count" ]) (Redaction.Action.mask mask));
  expect_typed_policy_error "empty variant replacement" typed_targets_schema
    (Redaction.Rule.at
       (Redaction.Path.root
       |> Redaction.Path.field "phase"
       |> Redaction.Path.case "Started")
       (Redaction.Action.replace (Observe.Value.string "replacement")));
  let table_schema =
    Observe.Schema.record table_target_t ~builder:(fun _ -> ())
  in
  expect_typed_policy_error "unstable hash-table index" table_schema
    (Redaction.Rule.at
       (Redaction.Path.root
       |> Redaction.Path.field "entries"
       |> Redaction.Path.index 0)
       Redaction.Action.remove)

let () =
  Alcotest.run "observe-redaction-contracts"
    [
      ( "behavior:observe:redaction",
        [
          Alcotest.test_case "default policy" `Quick
            test_default_policy_is_inert;
          Alcotest.test_case "exact paths" `Quick
            test_exact_nested_list_case_and_absent_paths;
          Alcotest.test_case "repeated wide contributions" `Quick
            test_repeated_wide_contributions;
          Alcotest.test_case "custom masks" `Quick
            test_custom_masks_and_failure_fallback;
          Alcotest.test_case "global matching" `Quick
            test_matching_structured_text_and_annotation;
          Alcotest.test_case "typed schema identity" `Quick
            test_typed_schema_identity_and_forms;
          Alcotest.test_case "opaque typed schema" `Quick
            test_opaque_typed_schema_defers_path_validation;
          Alcotest.test_case "open and typed wide" `Quick
            test_open_and_typed_wide_forms;
          Alcotest.test_case "replacement and composition" `Quick
            test_replacement_rechecked_and_rule_composition;
          Alcotest.test_case "lazy authoring" `Quick test_lazy_authoring;
          Alcotest.test_case "dynamic conflict and formatters" `Quick
            test_dynamic_conflict_fails_closed_and_formats_safely;
          Alcotest.test_case "overlapping contains" `Quick
            test_overlapping_contains_are_complete_and_order_independent;
          Alcotest.test_case "custom result validation" `Quick
            test_custom_invalid_and_over_budget_results_use_fallback;
          Alcotest.test_case "policy validation" `Quick
            test_policy_validation_rejects_impossible_actions;
          Alcotest.test_case "stricter drain" `Quick
            test_stricter_drain_only_reduces_disclosure;
        ] );
    ]
