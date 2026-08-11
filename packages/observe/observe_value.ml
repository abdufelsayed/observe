type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Object of (string * t) list
  | Embedded : 'a Repr.t * 'a -> t

let null = Null
let bool value = Bool value
let int value = Int value
let float value = Float value
let string value = String value
let option = function None -> Null | Some value -> value
let list values = List values
let object_ fields = Object fields
let embed type_ value = Embedded (type_, value)

let rec pp formatter = function
  | Null -> Format.pp_print_string formatter "null"
  | Bool value -> Format.pp_print_bool formatter value
  | Int value -> Format.pp_print_int formatter value
  | Float value -> Format.pp_print_float formatter value
  | String value -> Format.fprintf formatter "%S" value
  | List values ->
      Format.fprintf formatter "[@[<hov>%a@]]"
        (Format.pp_print_list
           ~pp_sep:(fun formatter () -> Format.fprintf formatter ";@ ")
           pp)
        values
  | Object fields ->
      Format.fprintf formatter "{@[<hov>%a@]}"
        (Format.pp_print_list
           ~pp_sep:(fun formatter () -> Format.fprintf formatter ";@ ")
           (fun formatter (name, value) ->
             Format.fprintf formatter "%S: %a" name pp value))
        fields
  | Embedded (type_, value) -> Repr.pp type_ formatter value

let to_string value = Format.asprintf "%a" pp value

type json_error = Invalid_utf8 | Non_finite_float | Unsupported_value

exception Json_error of json_error

let is_valid_utf8 value =
  let length = String.length value in
  let byte index = Char.code value.[index] in
  let is_continuation index =
    index < length
    &&
    let byte = byte index in
    byte >= 0x80 && byte <= 0xbf
  in
  let utf8_sequence_length index leading =
    if leading >= 0xc2 && leading <= 0xdf && is_continuation (index + 1) then
      Some 2
    else if leading = 0xe0 && index + 2 < length then
      let second = byte (index + 1) in
      if second >= 0xa0 && second <= 0xbf && is_continuation (index + 2) then
        Some 3
      else None
    else if
      ((leading >= 0xe1 && leading <= 0xec)
      || (leading >= 0xee && leading <= 0xef))
      && is_continuation (index + 1)
      && is_continuation (index + 2)
    then Some 3
    else if leading = 0xed && index + 2 < length then
      let second = byte (index + 1) in
      if second >= 0x80 && second <= 0x9f && is_continuation (index + 2) then
        Some 3
      else None
    else if leading = 0xf0 && index + 3 < length then
      let second = byte (index + 1) in
      if
        second >= 0x90
        && second <= 0xbf
        && is_continuation (index + 2)
        && is_continuation (index + 3)
      then Some 4
      else None
    else if
      leading >= 0xf1
      && leading <= 0xf3
      && is_continuation (index + 1)
      && is_continuation (index + 2)
      && is_continuation (index + 3)
    then Some 4
    else if leading = 0xf4 && index + 3 < length then
      let second = byte (index + 1) in
      if
        second >= 0x80
        && second <= 0x8f
        && is_continuation (index + 2)
        && is_continuation (index + 3)
      then Some 4
      else None
    else None
  in
  let rec check index =
    if index = length then true
    else
      let leading = byte index in
      if leading < 0x80 then check (index + 1)
      else
        match utf8_sequence_length index leading with
        | None -> false
        | Some sequence_length -> check (index + sequence_length)
  in
  check 0

let add_control_escape buffer byte =
  Buffer.add_string buffer (Printf.sprintf "\\u%04x" byte)

let add_json_string buffer value =
  if not (is_valid_utf8 value) then raise (Json_error Invalid_utf8);
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
      | character when Char.code character < 0x20 ->
          add_control_escape buffer (Char.code character)
      | character -> Buffer.add_char buffer character)
    value;
  Buffer.add_char buffer '"'

let add_json_float buffer value =
  match classify_float value with
  | FP_nan | FP_infinite -> raise (Json_error Non_finite_float)
  | FP_normal | FP_subnormal | FP_zero ->
      Buffer.add_string buffer (Printf.sprintf "%.17g" value)

let rec add_json buffer = function
  | Null -> Buffer.add_string buffer "null"
  | Bool value -> Buffer.add_string buffer (string_of_bool value)
  | Int value -> Buffer.add_string buffer (string_of_int value)
  | Float value -> add_json_float buffer value
  | String value -> add_json_string buffer value
  | List values ->
      Buffer.add_char buffer '[';
      List.iteri
        (fun index value ->
          if index <> 0 then Buffer.add_char buffer ',';
          add_json buffer value)
        values;
      Buffer.add_char buffer ']'
  | Object fields ->
      Buffer.add_char buffer '{';
      List.iteri
        (fun index (name, value) ->
          if index <> 0 then Buffer.add_char buffer ',';
          add_json_string buffer name;
          Buffer.add_char buffer ':';
          add_json buffer value)
        fields;
      Buffer.add_char buffer '}'
  | Embedded (type_, value) ->
      let encoded =
        try Repr.to_json_string ~minify:true type_ value
        with Repr.Unsupported_operation _ | Failure _ ->
          raise (Json_error Unsupported_value)
      in
      if not (is_valid_utf8 encoded) then raise (Json_error Invalid_utf8);
      Buffer.add_string buffer encoded

let to_json_string value =
  try
    let buffer = Buffer.create 128 in
    add_json buffer value;
    Ok (Buffer.contents buffer)
  with Json_error error -> Error error
