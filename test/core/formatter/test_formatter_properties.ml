module Observer = Observe.Make (Test_io.IO)

let observer = Observer.create (Test_io.Host.create ())

let count_from_env ~default =
  match Sys.getenv_opt "OBSERVE_QCHECK_COUNT" with
  | None | Some "" -> default
  | Some value -> (
      match int_of_string_opt value with
      | Some count when count > 0 -> count
      | _ -> default)

let ascii_string =
  QCheck.Gen.string_size
    ~gen:(QCheck.Gen.char_range '\000' '\127')
    (QCheck.Gen.int_range 0 64)

let text_case = QCheck.make (QCheck.Gen.pair ascii_string ascii_string)

let capture_text tag message =
  let config = Test_io.config "property" in
  match
    Observer.with_capture observer config (fun capture ->
        Observe.Logs.info (Observe.Logs.text ~tag message);
        match Observe.Capture.logs capture with
        | [ log ] -> log
        | _ -> failwith "expected one captured log")
  with
  | Ok log -> log
  | Error _ -> failwith "I/O implementation unexpectedly conflicted"

let capture_structure key value =
  let config = Test_io.config "property" in
  match
    Observer.with_capture observer config (fun capture ->
        Observe.Logs.info
          (Observe.Logs.free (fun () ->
               Observe.Value.object_ [ (key, Observe.Value.string value) ]));
        match Observe.Capture.logs capture with
        | [ log ] -> log
        | _ -> failwith "expected one captured log")
  with
  | Ok log -> log
  | Error _ -> failwith "I/O implementation unexpectedly conflicted"

let format style log =
  Observe.Formatter.format (Observe.Formatter.readable style) log

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

let has_raw_record_control value =
  String.exists (function '\027' | '\r' | '\n' -> true | _ -> false) value

let count character value =
  String.fold_left
    (fun total current -> if current = character then total + 1 else total)
    0 value

let prop_color_preserves_plain_and_controls_are_escaped =
  QCheck.Test.make ~count:(count_from_env ~default:300)
    ~name:"color strips to plain and caller controls stay escaped" text_case
    (fun (tag, message) ->
      let log = capture_text tag message in
      match format Observe.Formatter.Plain log with
      | Error _ -> false
      | Ok plain ->
          List.for_all
            (fun style ->
              match format style log with
              | Ok styled -> strip_ansi styled = plain
              | Error _ -> false)
            [
              Observe.Formatter.Ansi_16;
              Observe.Formatter.Ansi_256;
              Observe.Formatter.Truecolor;
            ]
          && not (has_raw_record_control plain))

let prop_structure_controls_are_escaped =
  QCheck.Test.make ~count:(count_from_env ~default:300)
    ~name:"structural keys and values cannot inject terminal controls" text_case
    (fun (key, value) ->
      match format Observe.Formatter.Plain (capture_structure key value) with
      | Ok output ->
          count '\n' output = 1
          && (not (String.contains output '\027'))
          && not (String.contains output '\r')
      | Error _ -> false)

let () =
  Alcotest.run "observe-formatter-properties"
    [
      ( "pbt:observe:formatter",
        [
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_color_preserves_plain_and_controls_are_escaped;
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_structure_controls_are_escaped;
        ] );
    ]
