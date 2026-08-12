type error = Invalid_utf8 | Non_finite_float | Unsupported_value | Failed
type style = Plain | Ansi_16 | Ansi_256 | Truecolor
type t = Observe_log.t -> (string, error) result

let create formatter = formatter
let format formatter log = formatter log
let nanoseconds_per_day = 86_400_000_000_000L

let time_of_day instant =
  let nanoseconds = Observe_instant.to_epoch_nanoseconds instant in
  let within_day = Int64.rem nanoseconds nanoseconds_per_day in
  let within_day =
    if Int64.compare within_day 0L < 0 then
      Int64.add within_day nanoseconds_per_day
    else within_day
  in
  let milliseconds = Int64.div within_day 1_000_000L in
  let hours = Int64.to_int (Int64.div milliseconds 3_600_000L) in
  let minutes = Int64.to_int (Int64.rem (Int64.div milliseconds 60_000L) 60L) in
  let seconds = Int64.to_int (Int64.rem (Int64.div milliseconds 1_000L) 60L) in
  let milliseconds = Int64.to_int (Int64.rem milliseconds 1_000L) in
  Printf.sprintf "%02d:%02d:%02d.%03d" hours minutes seconds milliseconds

let level_label level = String.uppercase_ascii (Observe_level.to_string level)
let ansi code value = "\027[" ^ code ^ "m" ^ value ^ "\027[0m"

type color = { ansi_16 : string; ansi_256 : int; rgb : int * int * int }

let metadata = { ansi_16 = "90"; ansi_256 = 244; rgb = (111, 119, 130) }
let debug = { ansi_16 = "90"; ansi_256 = 246; rgb = (139, 149, 167) }
let info = { ansi_16 = "96"; ansi_256 = 39; rgb = (14, 165, 233) }
let warn = { ansi_16 = "93"; ansi_256 = 178; rgb = (217, 149, 0) }
let error = { ansi_16 = "91"; ansi_256 = 203; rgb = (240, 82, 82) }
let constructor = { ansi_16 = "95"; ansi_256 = 170; rgb = (208, 107, 223) }
let field = { ansi_16 = "95"; ansi_256 = 141; rgb = (167, 139, 250) }
let string = { ansi_16 = "92"; ansi_256 = 78; rgb = (66, 184, 131) }
let number = { ansi_16 = "93"; ansi_256 = 214; rgb = (245, 158, 11) }
let boolean = { ansi_16 = "95"; ansi_256 = 213; rgb = (232, 121, 249) }

let styled ?(bold = false) style color value =
  let decorate code = if bold then "1;" ^ code else code in
  match style with
  | Plain -> value
  | Ansi_16 -> ansi (decorate color.ansi_16) value
  | Ansi_256 -> ansi (decorate (Printf.sprintf "38;5;%d" color.ansi_256)) value
  | Truecolor ->
      let red, green, blue = color.rgb in
      ansi (decorate (Printf.sprintf "38;2;%d;%d;%d" red green blue)) value

let level_color = function
  | Observe_level.Debug -> debug
  | Observe_level.Info -> info
  | Observe_level.Warn -> warn
  | Observe_level.Error -> error

let style_level style level value =
  styled ~bold:true style (level_color level) value

let style_metadata style value = styled style metadata value
let style_constructor style value = styled ~bold:true style constructor value
let style_field style value = styled style field value
let style_string style value = styled style string value
let style_number style value = styled style number value
let style_boolean style value = styled style boolean value

let terminal_text value =
  if not (Observe_value.is_valid_utf8 value) then Error Invalid_utf8
  else
    let buffer = Buffer.create (String.length value) in
    String.iter
      (function
        | '\b' -> Buffer.add_string buffer "\\b"
        | '\012' -> Buffer.add_string buffer "\\f"
        | '\n' -> Buffer.add_string buffer "\\n"
        | '\r' -> Buffer.add_string buffer "\\r"
        | '\t' -> Buffer.add_string buffer "\\t"
        | character when Char.code character < 0x20 || character = '\127' ->
            Buffer.add_string buffer
              (Printf.sprintf "\\u%04x" (Char.code character))
        | character -> Buffer.add_char buffer character)
      value;
    Ok (Buffer.contents buffer)

exception Readable_error of error

let readable_text value =
  match terminal_text value with
  | Ok value -> value
  | Error error -> raise (Readable_error error)

let quoted_text value =
  if not (Observe_value.is_valid_utf8 value) then
    raise (Readable_error Invalid_utf8)
  else
    let buffer = Buffer.create (String.length value + 2) in
    Buffer.add_char buffer '"';
    String.iter
      (function
        | '"' -> Buffer.add_string buffer "\\\""
        | '\\' -> Buffer.add_string buffer "\\\\"
        | '\b' -> Buffer.add_string buffer "\\b"
        | '\012' -> Buffer.add_string buffer "\\f"
        | '\n' -> Buffer.add_string buffer "\\n"
        | '\r' -> Buffer.add_string buffer "\\r"
        | '\t' -> Buffer.add_string buffer "\\t"
        | character when Char.code character < 0x20 || character = '\127' ->
            Buffer.add_string buffer
              (Printf.sprintf "\\u%04x" (Char.code character))
        | character -> Buffer.add_char buffer character)
      value;
    Buffer.add_char buffer '"';
    Buffer.contents buffer

let display_error = function
  | Observe_display.Invalid_utf8 -> Invalid_utf8
  | Observe_display.Non_finite_float -> Non_finite_float
  | Observe_display.Unsupported_value -> Unsupported_value
  | Observe_display.Malformed -> Failed

let ( let* ) = Result.bind

let rec display_of_value = function
  | Observe_value.Null -> Ok Observe_display.Null
  | Observe_value.Bool value -> Ok (Observe_display.Bool value)
  | Observe_value.Int value -> Ok (Observe_display.Number (string_of_int value))
  | Observe_value.Float value -> (
      match classify_float value with
      | FP_nan | FP_infinite -> Error Observe_display.Non_finite_float
      | FP_normal | FP_subnormal | FP_zero ->
          let encoded = string_of_float value in
          let encoded =
            if encoded.[String.length encoded - 1] = '.' then
              String.sub encoded 0 (String.length encoded - 1)
            else encoded
          in
          Ok (Observe_display.Number encoded))
  | Observe_value.String value ->
      Result.map
        (fun value -> Observe_display.String value)
        (Observe_display.valid_string value)
  | Observe_value.List values ->
      Result.map
        (fun values -> Observe_display.List values)
        (List.fold_right
           (fun value rest ->
             let* value = display_of_value value in
             let* rest = rest in
             Ok (value :: rest))
           values (Ok []))
  | Observe_value.Object fields ->
      let convert (name, value) =
        let* name = Observe_display.valid_string name in
        let* value = display_of_value value in
        Ok (name, value)
      in
      Result.map
        (fun fields -> Observe_display.Object fields)
        (List.fold_right
           (fun field rest ->
             let* field = convert field in
             let* rest = rest in
             Ok (field :: rest))
           fields (Ok []))
  | Observe_value.Embedded (description, value) ->
      Observe_type.present description value

let rec scalar style = function
  | Observe_display.Null -> Some (style_metadata style "null")
  | Observe_display.Bool value ->
      Some (style_boolean style (string_of_bool value))
  | Observe_display.Number value -> Some (style_number style value)
  | Observe_display.String value ->
      Some (style_string style (quoted_text value))
  | Observe_display.List [] -> Some (style_metadata style "[]")
  | Observe_display.List values -> (
      match scalar_values style values with
      | Some values -> Some ("[" ^ String.concat ", " values ^ "]")
      | None -> None)
  | Observe_display.Object [] | Observe_display.Record [] ->
      Some (style_metadata style "{}")
  | Observe_display.Object (_ :: _) | Observe_display.Record (_ :: _) -> None
  | Observe_display.Variant { name; polymorphic; payload = None } ->
      let name = if polymorphic then "`" ^ name else name in
      Some (style_constructor style (readable_text name))
  | Observe_display.Variant { payload = Some _; _ } -> None

and scalar_values style = function
  | [] -> Some []
  | value :: rest -> (
      match (scalar style value, scalar_values style rest) with
      | Some value, Some rest -> Some (value :: rest)
      | None, _ | _, None -> None)

type tree_label = Field of string | Constructor of string

let children = function
  | Observe_display.Object fields | Observe_display.Record fields ->
      List.map (fun (name, value) -> (Some (Field name), value)) fields
  | Observe_display.List values ->
      List.mapi
        (fun index value -> (Some (Field (Printf.sprintf "[%d]" index)), value))
        values
  | Observe_display.Variant { name; polymorphic; payload = Some payload } ->
      let name = if polymorphic then "`" ^ name else name in
      [ (Some (Constructor name), payload) ]
  | Observe_display.Null | Observe_display.Bool _ | Observe_display.Number _
  | Observe_display.String _
  | Observe_display.Variant { payload = None; _ } ->
      []

let render_tree style display =
  let buffer = Buffer.create 128 in
  let rec render_items prefix = function
    | [] -> ()
    | items ->
        let last_index = List.length items - 1 in
        List.iteri
          (fun index (label, value) ->
            let last = index = last_index in
            let connector = if last then "└─" else "├─" in
            let label =
              Option.map
                (function
                  | Field label -> `Field (readable_text label)
                  | Constructor label -> `Constructor (readable_text label))
                label
            in
            let scalar = scalar style value in
            Buffer.add_string buffer prefix;
            Buffer.add_string buffer (style_metadata style connector);
            Buffer.add_char buffer ' ';
            (match (label, scalar) with
            | Some (`Field label), Some scalar ->
                Buffer.add_string buffer (style_field style label);
                Buffer.add_string buffer (style_metadata style ":");
                Buffer.add_char buffer ' ';
                Buffer.add_string buffer scalar
            | Some (`Constructor label), Some scalar ->
                Buffer.add_string buffer (style_constructor style label);
                Buffer.add_string buffer (style_metadata style ":");
                Buffer.add_char buffer ' ';
                Buffer.add_string buffer scalar
            | Some (`Field label), None ->
                Buffer.add_string buffer (style_field style label)
            | Some (`Constructor label), None ->
                Buffer.add_string buffer (style_constructor style label)
            | None, Some scalar -> Buffer.add_string buffer scalar
            | None, None -> ());
            let nested =
              match scalar with Some _ -> [] | None -> children value
            in
            if nested <> [] then (
              Buffer.add_char buffer '\n';
              render_items (prefix ^ if last then "   " else "│  ") nested;
              if not last then Buffer.add_char buffer '\n')
            else if not last then Buffer.add_char buffer '\n')
          items
  in
  let items =
    match display with
    | Observe_display.Object []
    | Observe_display.Record []
    | Observe_display.List []
    | Observe_display.Null | Observe_display.Bool _ | Observe_display.Number _
    | Observe_display.String _
    | Observe_display.Variant { payload = None; _ } ->
        [ (None, display) ]
    | Observe_display.Object fields | Observe_display.Record fields ->
        List.map (fun (name, value) -> (Some (Field name), value)) fields
    | Observe_display.Variant { name; polymorphic; payload = Some payload } ->
        let name = if polymorphic then "`" ^ name else name in
        [ (Some (Constructor name), payload) ]
    | Observe_display.List values -> (
        match scalar style display with
        | Some _ -> [ (None, display) ]
        | None ->
            List.mapi
              (fun index value ->
                (Some (Field (Printf.sprintf "[%d]" index)), value))
              values)
  in
  render_items "  " items;
  Buffer.contents buffer

let structured_header style log =
  let level = Observe_log.level log in
  style_metadata style (time_of_day (Observe_log.instant log))
  ^ " "
  ^ style_level style level (level_label level)
  ^ " "
  ^ style_level style level ("[" ^ readable_text (Observe_log.service log) ^ "]")

let readable_display style log display =
  structured_header style log ^ "\n" ^ render_tree style display

let readable style =
  create (fun log ->
      try
        match Observe_log.payload log with
        | Text { tag; message } ->
            let level = Observe_log.level log in
            Ok
              (style_metadata style (time_of_day (Observe_log.instant log))
              ^ " "
              ^ style_level style level (level_label level)
              ^ " "
              ^ style_level style level ("[" ^ readable_text tag ^ "]")
              ^ " "
              ^ readable_text message)
        | Free value -> (
            match display_of_value value with
            | Ok display -> Ok (readable_display style log display)
            | Error error -> Error (display_error error))
        | Structured (description, value) -> (
            match Observe_type.present description value with
            | Ok display -> Ok (readable_display style log display)
            | Error error -> Error (display_error error))
      with Readable_error error -> Error error)

let json_error = function
  | Observe_value.Invalid_utf8 -> Invalid_utf8
  | Observe_value.Non_finite_float -> Non_finite_float
  | Observe_value.Unsupported_value -> Unsupported_value

let json_value value =
  Result.map_error json_error (Observe_value.to_json_string value)

let json_string value = json_value (Observe_value.string value)

let json_object fields =
  let buffer = Buffer.create 128 in
  Buffer.add_char buffer '{';
  List.iteri
    (fun index (name, encoded_value) ->
      if index <> 0 then Buffer.add_char buffer ',';
      Buffer.add_char buffer '"';
      Buffer.add_string buffer name;
      Buffer.add_string buffer "\":";
      Buffer.add_string buffer encoded_value)
    fields;
  Buffer.add_char buffer '}';
  Buffer.contents buffer

let text_json ~tag ~message =
  match json_string tag with
  | Error _ as error -> error
  | Ok tag -> (
      match json_string message with
      | Error _ as error -> error
      | Ok message ->
          Ok
            (json_object
               [ ("kind", "\"text\""); ("tag", tag); ("message", message) ]))

let repr_json description value =
  try
    let encoded = Observe_type.to_json_string ~minify:true description value in
    if Observe_value.is_valid_utf8 encoded then Ok encoded
    else Error Invalid_utf8
  with Repr.Unsupported_operation _ | Failure _ -> Error Unsupported_value

let payload_json log =
  match Observe_log.payload log with
  | Text { tag; message } -> text_json ~tag ~message
  | Free value -> json_value value
  | Structured (description, value) -> repr_json description value

let optional_string_field name = function
  | None -> Ok []
  | Some value -> (
      match json_string value with
      | Error _ as error -> error
      | Ok value -> Ok [ (name, value) ])

let encode_json log =
  match json_string (Observe_log.service log) with
  | Error _ as error -> error
  | Ok service -> (
      match
        optional_string_field "environment" (Observe_log.environment log)
      with
      | Error _ as error -> error
      | Ok environment -> (
          match optional_string_field "version" (Observe_log.version log) with
          | Error _ as error -> error
          | Ok version -> (
              match
                json_string
                  (Int64.to_string
                     (Observe_instant.to_epoch_nanoseconds
                        (Observe_log.instant log)))
              with
              | Error _ as error -> error
              | Ok instant -> (
                  match
                    json_string
                      (Observe_level.to_string (Observe_log.level log))
                  with
                  | Error _ as error -> error
                  | Ok level -> (
                      match payload_json log with
                      | Error _ as error -> error
                      | Ok payload ->
                          Ok
                            (json_object
                               ([ ("service", service) ]
                               @ environment
                               @ version
                               @ [
                                   ("instant", instant);
                                   ("level", level);
                                   ("payload", payload);
                                 ])))))))

let json = create encode_json

let json_lines =
  create (fun log ->
      match encode_json log with
      | Error _ as error -> error
      | Ok encoded -> Ok (encoded ^ "\n"))
