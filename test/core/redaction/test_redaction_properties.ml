module Observer = Observe.Make (Test_io.IO)
module Redaction = Observe.Logs.Redaction

let observer = Observer.create (Test_io.Host.create ())

let capture policy author =
  let config =
    Observe.Config.create_exn ~service:"redaction-property" ~environment:"test"
      ~console:Observe.Config.Silent ~min_level:Observe.Level.Debug
      ~redaction:policy ()
  in
  match
    Observer.with_capture observer ~config (fun capture ->
        author ();
        capture)
  with
  | Ok capture -> capture
  | Error Observe.IO_already_registered ->
      failwith "property capture route unexpectedly conflicted"
  | Error (Observe.Invalid_capacity capacity) ->
      failwith (Format.asprintf "invalid property capacity %d" capacity)
  | Error Observe.Runtime_closed -> failwith "property runtime was closed"

let one_log capture =
  match Observe.Capture.logs capture with
  | [ log ] -> log
  | logs ->
      failwith
        (Format.asprintf "expected one property log, received %d"
           (List.length logs))

let object_fields log =
  match Observe.Value.view (Observe.Log.fields log) with
  | `Object fields -> fields
  | _ -> failwith "property log did not have an object root"

let field name log =
  List.find_map
    (fun (candidate, value) ->
      if String.equal candidate name then Some value else None)
    (object_fields log)

let string_field name log =
  match field name log with
  | Some value -> (
      match Observe.Value.view value with
      | `String value -> value
      | _ ->
          failwith (Format.asprintf "property field %s was not a string" name))
  | None -> failwith (Format.asprintf "property field %s was absent" name)

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search offset =
    if offset + fragment_length > text_length then false
    else if String.sub text offset fragment_length = fragment then true
    else search (offset + 1)
  in
  fragment_length = 0 || search 0

let valid_json value =
  let decoder = Jsonm.decoder (`String value) in
  let rec decode saw_lexeme =
    match Jsonm.decode decoder with
    | `Lexeme _ -> decode true
    | `End -> saw_lexeme
    | `Await | `Error _ -> false
  in
  decode false

let json_fields log =
  Observe.Value.frozen_to_json_string (Observe.Log.fields log)

let global_policy =
  Redaction.create_exn
    ~rules:
      [
        Redaction.Rule.matching
          (Redaction.Matcher.string_prefix "secret-")
          (Redaction.Action.replace (Observe.Value.string "[safe]"));
      ]
    ()

let secret_case =
  let generator =
    QCheck.Gen.map
      (fun suffix -> "secret-" ^ suffix)
      (QCheck.Gen.string_size
         ~gen:(QCheck.Gen.char_range 'a' 'z')
         (QCheck.Gen.int_range 0 64))
  in
  QCheck.make ~print:(fun value -> Format.asprintf "%S" value) generator

let prop_global_matcher_never_leaks_generated_secret =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:200)
    ~name:"global replacement never leaks generated structured secrets"
    secret_case (fun secret ->
      let log =
        one_log
          (capture global_policy (fun () ->
               Observe.Logs.info (fun m ->
                   let open Observe.Logs in
                   m.untyped
                   |+ m.field "token" Observe.Type.string secret
                   |> m.seal)))
      in
      String.equal (string_field "token" log) "[safe]"
      && (not (contains (json_fields log) secret))
      && List.length (Observe.Log.redactions log) = 1)

let utf8_prefix =
  let pieces = [ "a"; "é"; "λ"; "🙂"; "中"; "🦄" ] in
  QCheck.Gen.map
    (fun values -> String.concat "" values)
    (QCheck.Gen.list_size
       (QCheck.Gen.int_range 1 16)
       (QCheck.Gen.oneof_list pieces))

let mask_case =
  QCheck.make
    ~print:(fun value -> Format.asprintf "%S" value)
    (QCheck.Gen.map (fun prefix -> prefix ^ "1234") utf8_prefix)

let prop_finite_masks_are_utf8_safe_and_preserve_suffix =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:200)
    ~name:"finite masks preserve a scalar suffix and valid JSON" mask_case
    (fun input ->
      let mask =
        Redaction.Mask.keep_suffix ~characters:4
          ~hidden:(Redaction.Mask.Fill "*") ()
      in
      let policy =
        Redaction.create_exn
          ~rules:
            [
              Redaction.Rule.at
                (Redaction.Path.fields [ "token" ])
                (Redaction.Action.mask mask);
            ]
          ()
      in
      let log =
        one_log
          (capture policy (fun () ->
               Observe.Logs.info (fun m ->
                   let open Observe.Logs in
                   m.untyped
                   |+ m.field "token" Observe.Type.string input
                   |> m.seal)))
      in
      let actual = string_field "token" log in
      let suffix = String.sub actual (String.length actual - 4) 4 in
      String.equal suffix "1234"
      && (not (String.equal actual input))
      && valid_json (json_fields log))

let prop_exact_removal_is_local =
  let values =
    QCheck.Gen.pair
      (QCheck.Gen.string_size
         ~gen:(QCheck.Gen.char_range 'a' 'z')
         (QCheck.Gen.int_range 0 48))
      (QCheck.Gen.string_size
         ~gen:(QCheck.Gen.char_range 'a' 'z')
         (QCheck.Gen.int_range 0 48))
  in
  let case =
    QCheck.make
      ~print:(fun (secret, safe) ->
        Format.asprintf "secret=%S safe=%S" secret safe)
      values
  in
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:200)
    ~name:"exact removal never removes an unrelated sibling" case
    (fun (secret, safe) ->
      let policy =
        Redaction.create_exn
          ~rules:
            [
              Redaction.Rule.at
                (Redaction.Path.fields [ "secret" ])
                Redaction.Action.remove;
            ]
          ()
      in
      let log =
        one_log
          (capture policy (fun () ->
               Observe.Logs.info (fun m ->
                   let open Observe.Logs in
                   m.untyped
                   |+ m.field "secret" Observe.Type.string secret
                   |+ m.field "safe" Observe.Type.string safe
                   |> m.seal)))
      in
      let safe_present =
        match field "safe" log with
        | Some value -> (
            match Observe.Value.view value with
            | `String actual -> String.equal actual safe
            | _ -> false)
        | None -> false
      in
      safe_present && Option.is_none (field "secret" log))

let prop_authored_source_is_not_mutated =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:100)
    ~name:"redaction does not mutate an authored source cell" secret_case
    (fun secret ->
      let source = ref secret in
      let log =
        one_log
          (capture global_policy (fun () ->
               Observe.Logs.info (fun m ->
                   let open Observe.Logs in
                   m.untyped
                   |+ m.field "token" Observe.Type.string !source
                   |> m.seal)))
      in
      source := "changed-after-authorship";
      String.equal (string_field "token" log) "[safe]")

let () =
  Alcotest.run "observe-redaction-properties"
    [
      ( "pbt:observe:redaction",
        [
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_global_matcher_never_leaks_generated_secret;
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_finite_masks_are_utf8_safe_and_preserve_suffix;
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_exact_removal_is_local;
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_authored_source_is_not_mutated;
        ] );
    ]
