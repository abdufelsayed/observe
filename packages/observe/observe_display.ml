type t =
  | Null
  | Bool of bool
  | Number of string
  | String of string
  | List of t list
  | Object of (string * t) list
  | Record of (string * t) list
  | Variant of { name : string; polymorphic : bool; payload : t option }

type error = Invalid_utf8 | Non_finite_float | Unsupported_value | Malformed

let ( let* ) = Result.bind

let valid_string value =
  if Observe_utf8.is_valid value then Ok value else Error Invalid_utf8

let rec map_list convert = function
  | [] -> Ok []
  | value :: rest ->
      let* value = convert value in
      let* rest = map_list convert rest in
      Ok (value :: rest)

let number value =
  match classify_float value with
  | FP_nan | FP_infinite -> Error Non_finite_float
  | FP_normal | FP_subnormal | FP_zero ->
      let encoded = string_of_float value in
      let encoded =
        if encoded.[String.length encoded - 1] = '.' then
          String.sub encoded 0 (String.length encoded - 1)
        else encoded
      in
      Ok (Number encoded)

let next decoder =
  match Repr.Json.decode decoder with
  | `Lexeme lexeme -> Ok lexeme
  | `Await | `End | `Error _ -> Error Malformed

let rec decode_value decoder =
  let* lexeme = next decoder in
  decode_lexeme decoder lexeme

and decode_lexeme decoder = function
  | `Null -> Ok Null
  | `Bool value -> Ok (Bool value)
  | `Float value -> number value
  | `String value ->
      let* value = valid_string value in
      Ok (String value)
  | `As -> decode_array decoder []
  | `Os -> decode_object decoder []
  | `Ae | `Oe | `Name _ -> Error Malformed

and decode_array decoder values =
  let* lexeme = next decoder in
  match lexeme with
  | `Ae -> Ok (List (List.rev values))
  | lexeme ->
      let* value = decode_lexeme decoder lexeme in
      decode_array decoder (value :: values)

and decode_object decoder fields =
  let* lexeme = next decoder in
  match lexeme with
  | `Oe -> Ok (Object (List.rev fields))
  | `Name name ->
      let* name = valid_string name in
      let* value = decode_value decoder in
      decode_object decoder ((name, value) :: fields)
  | `Null | `Bool _ | `Float _ | `String _ | `As | `Ae | `Os -> Error Malformed

let of_repr description value =
  try
    let encoded = Repr.to_json_string ~minify:true description value in
    let decoder = Repr.Json.decoder (`String encoded) in
    let* value = decode_value decoder in
    match Repr.Json.decode decoder with
    | `End -> Ok value
    | `Await | `Error _ | `Lexeme _ -> Error Malformed
  with Repr.Unsupported_operation _ | Failure _ -> Error Unsupported_value
