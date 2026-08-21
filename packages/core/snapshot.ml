type t =
  | Null
  | Bool of bool
  | Integer of string
  | Float of float
  | String of string
  | List of t list
  | Object of (string * t) list
  | Variant of { name : string; polymorphic : bool; payload : t option }

type error = Limit_exceeded | Invalid_utf8 | Unsupported | Conversion_failed

type context = {
  mutable nodes : int;
  mutable string_bytes : int;
  mutable byte_bytes : int;
  mutable retained_bytes : int;
}

let max_depth = 64
let width_limit = 1_024
let max_nodes = 100_000
let max_string_bytes = 1_048_576
let max_byte_bytes = 1_048_576
let max_retained_bytes = 4_194_304

let create_context () =
  { nodes = 0; string_bytes = 0; byte_bytes = 0; retained_bytes = 0 }

let check_depth ~depth =
  if depth > max_depth then Error Limit_exceeded else Ok ()

let reserve context ~depth ~retained =
  if depth > max_depth || context.nodes >= max_nodes then Error Limit_exceeded
  else if retained < 0 || context.retained_bytes > max_retained_bytes - retained
  then Error Limit_exceeded
  else (
    context.nodes <- context.nodes + 1;
    context.retained_bytes <- context.retained_bytes + retained;
    Ok ())

let copy_string value = Bytes.to_string (Bytes.of_string value)

let null context ~depth =
  Result.map (fun () -> Null) (reserve context ~depth ~retained:1)

let bool context ~depth value =
  Result.map (fun () -> Bool value) (reserve context ~depth ~retained:1)

let integer context ~depth value =
  let length = String.length value in
  match reserve context ~depth ~retained:length with
  | Error _ as error -> error
  | Ok () -> Ok (Integer (copy_string value))

let float context ~depth value =
  match classify_float value with
  | FP_nan | FP_infinite -> Error Conversion_failed
  | FP_normal | FP_subnormal | FP_zero ->
      Result.map (fun () -> Float value) (reserve context ~depth ~retained:8)

let copy_text context ~depth value =
  let length = String.length value in
  if not (Utf8.is_valid value) then Error Invalid_utf8
  else if
    length > max_string_bytes
    || context.string_bytes > max_string_bytes - length
  then Error Limit_exceeded
  else
    match reserve context ~depth ~retained:length with
    | Error _ as error -> error
    | Ok () ->
        context.string_bytes <- context.string_bytes + length;
        Ok (copy_string value)

let string context ~depth value =
  Result.map (fun value -> String value) (copy_text context ~depth value)

let bytes context ~depth value =
  let length = Bytes.length value in
  if length > max_byte_bytes || context.byte_bytes > max_byte_bytes - length
  then Error Limit_exceeded
  else
    let copied = Bytes.to_string (Bytes.copy value) in
    if not (Utf8.is_valid copied) then Error Invalid_utf8
    else
      match reserve context ~depth ~retained:length with
      | Error _ as error -> error
      | Ok () ->
          context.byte_bytes <- context.byte_bytes + length;
          Ok (String copied)

let width values =
  let rec count total = function
    | [] -> Ok total
    | _ :: rest when total < width_limit -> count (total + 1) rest
    | _ -> Error Limit_exceeded
  in
  count 0 values

let list context ~depth values =
  match width values with
  | Error _ as error -> error
  | Ok length ->
      Result.map
        (fun () -> List values)
        (reserve context ~depth ~retained:(8 * length))

let object_ context ~depth fields =
  match width fields with
  | Error _ as error -> error
  | Ok length -> (
      let rec copy_names retained copied = function
        | [] -> Ok (retained, List.rev copied)
        | (name, value) :: rest ->
            let size = String.length name in
            if not (Utf8.is_valid name) then Error Invalid_utf8
            else if retained > max_string_bytes - size then Error Limit_exceeded
            else
              copy_names (retained + size)
                ((copy_string name, value) :: copied)
                rest
      in
      match copy_names context.string_bytes [] fields with
      | Error _ as error -> error
      | Ok (string_bytes, fields) -> (
          let name_bytes = string_bytes - context.string_bytes in
          match
            reserve context ~depth ~retained:((16 * length) + name_bytes)
          with
          | Error _ as error -> error
          | Ok () ->
              context.string_bytes <- string_bytes;
              Ok (Object fields)))

let variant context ~depth ~polymorphic name payload =
  let length = String.length name in
  if not (Utf8.is_valid name) then Error Invalid_utf8
  else if
    length > max_string_bytes
    || context.string_bytes > max_string_bytes - length
  then Error Limit_exceeded
  else
    match reserve context ~depth ~retained:(16 + length) with
    | Error _ as error -> error
    | Ok () ->
        context.string_bytes <- context.string_bytes + length;
        Ok (Variant { name = copy_string name; polymorphic; payload })

let refreeze_into context value =
  let rec copy ~depth value =
    match check_depth ~depth with
    | Error _ as error -> error
    | Ok () -> (
        match value with
        | Null -> null context ~depth
        | Bool value -> bool context ~depth value
        | Integer value -> integer context ~depth value
        | Float value -> float context ~depth value
        | String value -> string context ~depth value
        | List values ->
            let rec collect count copied = function
              | [] -> list context ~depth (List.rev copied)
              | _ when count >= width_limit -> Error Limit_exceeded
              | value :: rest -> (
                  match copy ~depth:(depth + 1) value with
                  | Error _ as error -> error
                  | Ok value -> collect (count + 1) (value :: copied) rest)
            in
            collect 0 [] values
        | Object fields ->
            let rec collect count copied = function
              | [] -> object_ context ~depth (List.rev copied)
              | _ when count >= width_limit -> Error Limit_exceeded
              | (name, value) :: rest -> (
                  match copy ~depth:(depth + 1) value with
                  | Error _ as error -> error
                  | Ok value ->
                      collect (count + 1) ((name, value) :: copied) rest)
            in
            collect 0 [] fields
        | Variant { name; polymorphic; payload = None } ->
            variant context ~depth ~polymorphic name None
        | Variant { name; polymorphic; payload = Some payload } -> (
            match copy ~depth:(depth + 1) payload with
            | Error _ as error -> error
            | Ok payload ->
                variant context ~depth ~polymorphic name (Some payload)))
  in
  copy ~depth:0 value

let refreeze value = refreeze_into (create_context ()) value

let rec merge_fields existing patch =
  let merge_value previous next =
    match (previous, next) with
    | Object previous, Object next -> Object (merge_fields previous next)
    | _, next -> next
  in
  let update fields (name, next) =
    let rec loop prefix = function
      | [] -> List.rev_append prefix [ (name, next) ]
      | (candidate, previous) :: rest when String.equal candidate name ->
          List.rev_append prefix ((candidate, merge_value previous next) :: rest)
      | field :: rest -> loop (field :: prefix) rest
    in
    loop [] fields
  in
  List.fold_left update existing patch

let merge_object previous patch =
  match (previous, patch) with
  | Object previous, Object patch ->
      refreeze (Object (merge_fields previous patch))
  | _, _ -> Error Conversion_failed

let rec append_json buffer = function
  | Null -> Json_writer.null buffer
  | Bool value -> Json_writer.bool buffer value
  | Integer value -> Buffer.add_string buffer value
  | Float value -> Json_writer.float buffer value
  | String value -> Json_writer.string buffer value
  | List values ->
      Buffer.add_char buffer '[';
      List.iteri
        (fun index value ->
          if index > 0 then Buffer.add_char buffer ',';
          append_json buffer value)
        values;
      Buffer.add_char buffer ']'
  | Object fields ->
      Buffer.add_char buffer '{';
      List.iteri
        (fun index (name, value) ->
          if index > 0 then Buffer.add_char buffer ',';
          Json_writer.name buffer name;
          append_json buffer value)
        fields;
      Buffer.add_char buffer '}'
  | Variant { name; payload = None; _ } -> Json_writer.string buffer name
  | Variant { name; payload = Some payload; _ } ->
      Buffer.add_char buffer '{';
      Json_writer.name buffer name;
      append_json buffer payload;
      Buffer.add_char buffer '}'

open Pretty

let rec plan = function
  | Null -> Scalar (fun renderer -> null renderer)
  | Bool value -> Scalar (fun renderer -> bool renderer value)
  | Integer value -> Scalar (fun renderer -> number renderer value)
  | Float value ->
      Scalar
        (fun renderer -> number renderer (Json_writer.float_to_string value))
  | String value -> Scalar (fun renderer -> string renderer value)
  | List [] -> Scalar (fun renderer -> empty_list renderer)
  | List values ->
      let plans = List.map plan values in
      if List.for_all (function Scalar _ -> true | Node _ -> false) plans then
        Scalar
          (fun renderer ->
            list_start renderer;
            let rec append = function
              | [] -> ()
              | [ Scalar write ] -> write renderer
              | Scalar write :: rest ->
                  write renderer;
                  list_separator renderer;
                  append rest
              | Node _ :: _ -> assert false
            in
            append plans;
            list_end renderer)
      else
        let last = List.length plans - 1 in
        Node
          (fun renderer placement ->
            let nested = place renderer placement ~scalar:false in
            List.iteri
              (fun index planned ->
                render renderer (Index { last = index = last; index }) planned;
                if index < last then newline renderer)
              plans;
            finish renderer nested)
  | Object [] -> Scalar (fun renderer -> empty_record renderer)
  | Object fields ->
      let last = List.length fields - 1 in
      Node
        (fun renderer placement ->
          let nested = place renderer placement ~scalar:false in
          List.iteri
            (fun index (name, value) ->
              render renderer (Field { last = index = last; name }) (plan value);
              if index < last then newline renderer)
            fields;
          finish renderer nested)
  | Variant { name; polymorphic; payload = None } ->
      Scalar (fun renderer -> variant renderer ~polymorphic name)
  | Variant { name; polymorphic; payload = Some payload } ->
      Node
        (fun renderer placement ->
          let name = if polymorphic then "`" ^ name else name in
          let nested = place renderer placement ~scalar:false in
          render renderer (Constructor { last = true; name }) (plan payload);
          finish renderer nested)

let append_pretty renderer placement value =
  render renderer placement (plan value)
