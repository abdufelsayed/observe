module Observer = Observe.Make (Test_io.IO)

let observer = Observer.create (Test_io.Host.create ())

let ascii_string =
  QCheck.Gen.string_size
    ~gen:(QCheck.Gen.char_range '\000' '\127')
    (QCheck.Gen.int_range 0 32)

let valid_string =
  QCheck.Gen.oneof_weighted
    [
      (8, ascii_string);
      ( 2,
        QCheck.Gen.oneof_list
          [ "مرحبا"; "λ"; "🙂"; "line\nbreak"; "quote\"slash\\" ] );
    ]

let text_case = QCheck.make (QCheck.Gen.pair valid_string valid_string)

type sample =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of sample list
  | Object of (string * sample) list

let rec value_of_sample = function
  | Null -> Observe.Value.null
  | Bool value -> Observe.Value.bool value
  | Int value -> Observe.Value.int value
  | Float value -> Observe.Value.float value
  | String value -> Observe.Value.string value
  | List values -> Observe.Value.list (List.map value_of_sample values)
  | Object fields ->
      Observe.Value.object_
        (List.map (fun (name, value) -> (name, value_of_sample value)) fields)

let scalar_sample =
  let open QCheck.Gen in
  oneof
    [
      return Null;
      map (fun value -> Bool value) bool;
      map (fun value -> Int value) (int_range (-10_000) 10_000);
      map
        (fun value -> Float (float_of_int value /. 10.))
        (int_range (-10_000) 10_000);
      map (fun value -> String value) valid_string;
    ]

let rec sample_gen depth =
  let open QCheck.Gen in
  if depth = 0 then scalar_sample
  else
    let child = sample_gen (depth - 1) in
    oneof_weighted
      [
        (6, scalar_sample);
        (2, map (fun values -> List values) (list_size (int_range 0 4) child));
        ( 2,
          map
            (fun fields -> Object fields)
            (list_size (int_range 0 4) (pair valid_string child)) );
      ]

let shrink_valid_string value =
  if String.length value = 0 then QCheck.Iter.empty else QCheck.Iter.return ""

let rec shrink_sample = function
  | Null -> QCheck.Iter.empty
  | Bool value ->
      QCheck.Iter.map (fun value -> Bool value) (QCheck.Shrink.bool value)
  | Int value ->
      QCheck.Iter.map (fun value -> Int value) (QCheck.Shrink.int value)
  | Float value ->
      QCheck.Iter.map (fun value -> Float value) (QCheck.Shrink.float value)
  | String value ->
      QCheck.Iter.map (fun value -> String value) (shrink_valid_string value)
  | List values ->
      QCheck.Iter.append (QCheck.Iter.return Null)
        (QCheck.Iter.map
           (fun values -> List values)
           (QCheck.Shrink.list ~shrink:shrink_sample values))
  | Object fields ->
      let shrink_field = QCheck.Shrink.pair shrink_valid_string shrink_sample in
      QCheck.Iter.append (QCheck.Iter.return Null)
        (QCheck.Iter.map
           (fun fields -> Object fields)
           (QCheck.Shrink.list ~shrink:shrink_field fields))

let sample =
  QCheck.make ~shrink:shrink_sample
    ~print:(fun sample -> Observe.Value.to_string (value_of_sample sample))
    (sample_gen 3)

let capture_outcome message =
  let config = Test_io.config "property" in
  match
    Observer.with_capture observer config (fun capture ->
        Observe.Logs.info message;
        (Observe.Capture.logs capture, Observe.Capture.diagnostics capture))
  with
  | Ok outcome -> outcome
  | Error _ -> failwith "I/O implementation unexpectedly conflicted"

let capture message =
  match capture_outcome message with
  | [ log ], _ -> log
  | _ -> failwith "expected one captured log"

let capture_text tag message = capture (Test_io.text ~tag message)
let capture_value value = capture (fun m -> m.value value)
let format formatter log = Observe.Formatter.format formatter log
let pretty style log = format (Observe.Formatter.pretty style) log

let strip_ansi value =
  let buffer = Buffer.create (String.length value) in
  let length = String.length value in
  let rec copy index =
    if index = length then Buffer.contents buffer
    else if
      index + 1 < length && value.[index] = '\027' && value.[index + 1] = '['
    then skip (index + 2)
    else (
      Buffer.add_char buffer value.[index];
      copy (index + 1))
  and skip index =
    if index = length then Buffer.contents buffer
    else if value.[index] = 'm' then copy (index + 1)
    else skip (index + 1)
  in
  copy 0

let valid_json value =
  let decoder = Jsonm.decoder (`String value) in
  let rec decode saw_lexeme =
    match Jsonm.decode decoder with
    | `Lexeme _ -> decode true
    | `End -> saw_lexeme
    | `Await | `Error _ -> false
  in
  decode false

let has_raw_terminal_control value =
  String.exists (function '\027' | '\r' -> true | _ -> false) value

let count character value =
  String.fold_left
    (fun total current -> if current = character then total + 1 else total)
    0 value

let prop_color_preserves_plain_and_controls_are_escaped =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:300)
    ~name:"color strips to plain and caller controls stay escaped" text_case
    (fun (tag, message) ->
      let log = capture_text tag message in
      match pretty Observe.Formatter.Plain log with
      | Error _ -> false
      | Ok plain ->
          List.for_all
            (fun style ->
              match pretty style log with
              | Ok styled -> strip_ansi styled = plain
              | Error _ -> false)
            [
              Observe.Formatter.Ansi_16;
              Observe.Formatter.Ansi_256;
              Observe.Formatter.Truecolor;
            ]
          && (not (has_raw_terminal_control plain))
          && not (String.contains plain '\n'))

let prop_rich_values_have_equivalent_pretty_and_valid_json_projections =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:300)
    ~name:"rich values preserve pretty styling and form one valid JSON value"
    sample (fun sample ->
      let log = capture_value (value_of_sample sample) in
      match
        ( pretty Observe.Formatter.Plain log,
          format Observe.Formatter.json log,
          format Observe.Formatter.ndjson log )
      with
      | Ok plain, Ok json, Ok ndjson ->
          List.for_all
            (fun style ->
              match pretty style log with
              | Ok styled -> strip_ansi styled = plain
              | Error _ -> false)
            [
              Observe.Formatter.Ansi_16;
              Observe.Formatter.Ansi_256;
              Observe.Formatter.Truecolor;
            ]
          && (not (has_raw_terminal_control plain))
          && valid_json json
          && ndjson = json ^ "\n"
          && count '\n' ndjson = 1
      | Error _, _, _ | _, Error _, _ | _, _, Error _ -> false)

let invalid_utf8 =
  QCheck.make
    ~print:(fun value -> Format.asprintf "0x%02x" (Char.code value.[0]))
    (QCheck.Gen.map
       (fun byte -> String.make 1 (Char.chr byte))
       (QCheck.Gen.int_range 0x80 0xff))

let canonical_failure diagnostics =
  List.exists
    (fun (entry : Observe.Diagnostics.entry) ->
      entry.kind = Observe.Diagnostics.Canonical_freeze_failed
      && entry.count = 1)
    diagnostics

let withheld value =
  match capture_outcome (fun m -> m.value value) with
  | [], diagnostics -> canonical_failure diagnostics
  | _ -> false

let prop_invalid_utf8_is_withheld_at_the_canonical_boundary =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:128)
    ~name:"invalid UTF-8 is rejected in untyped keys and values" invalid_utf8
    (fun invalid ->
      withheld
        (Observe.Value.object_ [ (invalid, Observe.Value.string "value") ])
      && withheld
           (Observe.Value.object_ [ ("key", Observe.Value.string invalid) ]))

let test_non_finite_floats_are_withheld () =
  List.iter
    (fun value ->
      Alcotest.(check bool)
        "canonical freezing rejects non-finite float" true
        (withheld (Observe.Value.float value)))
    [ Float.nan; Float.infinity; Float.neg_infinity ]

let test_finite_float_matches_repr_precision () =
  let value = 1.2345678901234567 in
  let expected =
    "{\"service\":\"property\",\"timestamp\":\"42\",\"level\":\"info\",\"body\":"
    ^ Repr.to_json_string ~minify:true Repr.float value
    ^ "}"
  in
  Alcotest.(check (result string reject))
    "untyped float uses the Repr/Jsonm representation" (Ok expected)
    (format Observe.Formatter.json (capture_value (Observe.Value.float value)))

let () =
  Alcotest.run "observe-formatter-properties"
    [
      ( "pbt:observe:formatter",
        [
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_color_preserves_plain_and_controls_are_escaped;
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_rich_values_have_equivalent_pretty_and_valid_json_projections;
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_invalid_utf8_is_withheld_at_the_canonical_boundary;
        ] );
      ( "unit:observe:formatter-boundaries",
        [
          Alcotest.test_case "non-finite floats are withheld" `Quick
            test_non_finite_floats_are_withheld;
          Alcotest.test_case "finite float precision" `Quick
            test_finite_float_matches_repr_precision;
        ] );
    ]
