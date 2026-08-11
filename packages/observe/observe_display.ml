type t =
  | Null
  | Bool of bool
  | Number of string
  | String of string
  | List of t list
  | Object of (string * t) list

type error = Invalid_utf8 | Non_finite_float | Unsupported_value | Malformed

let ( let* ) = Result.bind

let valid_string value =
  if Observe_value.is_valid_utf8 value then Ok value else Error Invalid_utf8

let rec map_list convert = function
  | [] -> Ok []
  | value :: rest ->
      let* value = convert value in
      let* rest = map_list convert rest in
      Ok (value :: rest)

let rec of_json = function
  | `Null -> Ok Null
  | `Bool value -> Ok (Bool value)
  | `Int value -> Ok (Number (string_of_int value))
  | `Intlit value -> Ok (Number value)
  | `Float value -> (
      match classify_float value with
      | FP_nan | FP_infinite -> Error Non_finite_float
      | FP_normal | FP_subnormal | FP_zero ->
          Ok (Number (Yojson.Safe.to_string (`Float value))))
  | `String value ->
      let* value = valid_string value in
      Ok (String value)
  | `List values | `Tuple values ->
      let* values = map_list of_json values in
      Ok (List values)
  | `Assoc fields ->
      let convert (name, value) =
        let* name = valid_string name in
        let* value = of_json value in
        Ok (name, value)
      in
      let* fields = map_list convert fields in
      Ok (Object fields)
  | `Variant _ -> Error Malformed

let of_repr description value =
  try
    let encoded = Repr.to_json_string ~minify:true description value in
    if not (Observe_value.is_valid_utf8 encoded) then Error Invalid_utf8
    else
      try of_json (Yojson.Safe.from_string encoded)
      with Yojson.Json_error _ -> Error Malformed
  with Repr.Unsupported_operation _ | Failure _ -> Error Unsupported_value

let rec of_value = function
  | Observe_value.Null -> Ok Null
  | Observe_value.Bool value -> Ok (Bool value)
  | Observe_value.Int value -> Ok (Number (string_of_int value))
  | Observe_value.Float value -> (
      match classify_float value with
      | FP_nan | FP_infinite -> Error Non_finite_float
      | FP_normal | FP_subnormal | FP_zero ->
          Ok (Number (Yojson.Safe.to_string (`Float value))))
  | Observe_value.String value ->
      let* value = valid_string value in
      Ok (String value)
  | Observe_value.List values ->
      let* values = map_list of_value values in
      Ok (List values)
  | Observe_value.Object fields ->
      let convert (name, value) =
        let* name = valid_string name in
        let* value = of_value value in
        Ok (name, value)
      in
      let* fields = map_list convert fields in
      Ok (Object fields)
  | Observe_value.Embedded (description, value) -> of_repr description value
