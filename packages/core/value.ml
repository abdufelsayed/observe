type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Object of (string * t) list
  | Embedded : 'a Type.t * 'a -> t

type frozen = Snapshot.t

type integer =
  [ `Int of int | `Int32 of int32 | `Int64 of int64 | `Decimal of string ]

type truncation =
  | Depth
  | Object_fields
  | Collection
  | String_bytes
  | Bytes_length
  | Nodes
  | Total_bytes

type frozen_view =
  [ `Null
  | `Bool of bool
  | `Integer of integer
  | `Float of float
  | `String of string
  | `Bytes of string
  | `Truncated of truncation
  | `Truncated_list of frozen list * truncation
  | `Truncated_object of (string * frozen) list * truncation
  | `List of frozen list
  | `Object of (string * frozen) list
  | `Variant of string * bool * frozen option ]

let find path value =
  let rec field named = function
    | [] -> None
    | (name, value) :: rest ->
        if String.equal name named then Some value else field named rest
  in
  let rec descend path value =
    match path with
    | [] -> Some value
    | name :: rest -> (
        match Snapshot.view value with
        | `Object fields | `Truncated_object (fields, _) -> (
            match field name fields with
            | None -> None
            | Some value -> descend rest value)
        | `Null | `Bool _ | `Integer _ | `Float _ | `String _ | `Bytes _
        | `Truncated _ | `Truncated_list _ | `List _ | `Variant _ ->
            None)
  in
  descend path value

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
  | Embedded (type_, value) -> Type.pp type_ formatter value

let to_string value = Format.asprintf "%a" pp value

open Pretty

let rec plan value =
  match value with
  | Null -> Scalar (fun renderer -> null renderer)
  | Bool value -> Scalar (fun renderer -> bool renderer value)
  | Int value -> Scalar (fun renderer -> int renderer value)
  | Float value -> Scalar (fun renderer -> float renderer value)
  | String value -> Scalar (fun renderer -> string renderer value)
  | List values -> plan_list values
  | Object [] -> Scalar (fun renderer -> empty_record renderer)
  | Object fields ->
      Node
        (fun renderer placement ->
          let nested = place renderer placement ~scalar:false in
          append_fields renderer fields;
          finish renderer nested)
  | Embedded (description, value) -> Type.plan description value

and plan_list values =
  match List.map plan values with
  | [] -> Scalar (fun renderer -> empty_list renderer)
  | plans
    when List.for_all (function Scalar _ -> true | Node _ -> false) plans ->
      Scalar
        (fun renderer ->
          list_start renderer;
          append_scalars renderer plans;
          list_end renderer)
  | plans ->
      Node
        (fun renderer placement ->
          let nested = place renderer placement ~scalar:false in
          append_indexed renderer 0 plans;
          finish renderer nested)

and append_scalars renderer = function
  | [] -> ()
  | [ Scalar append ] -> append renderer
  | Scalar append :: rest ->
      append renderer;
      list_separator renderer;
      append_scalars renderer rest
  | Node _ :: _ -> invalid_arg "Observe.Value: non-scalar plan in scalar list"

and append_indexed renderer index = function
  | [] -> ()
  | [ planned ] -> render renderer (Index { last = true; index }) planned
  | planned :: rest ->
      render renderer (Index { last = false; index }) planned;
      newline renderer;
      append_indexed renderer (index + 1) rest

and append_fields renderer = function
  | [] -> ()
  | [ (name, value) ] ->
      render renderer (Field { last = true; name }) (plan value)
  | (name, value) :: rest ->
      render renderer (Field { last = false; name }) (plan value);
      newline renderer;
      append_fields renderer rest

let append_pretty renderer placement value =
  render renderer placement (plan value)

type json_error = Invalid_utf8 | Non_finite_float | Unsupported_value

exception Json_error of json_error

let add_json_float buffer value =
  match classify_float value with
  | FP_nan | FP_infinite -> raise (Json_error Non_finite_float)
  | FP_normal | FP_subnormal | FP_zero ->
      Buffer.add_string buffer (Json_writer.float_to_string value)

let rec add_json buffer = function
  | Null -> Json_writer.null buffer
  | Bool value -> Buffer.add_string buffer (string_of_bool value)
  | Int value -> Json_writer.decimal_int buffer value
  | Float value -> add_json_float buffer value
  | String value -> Json_writer.string buffer value
  | List values ->
      Buffer.add_char buffer '[';
      let rec add_values = function
        | [] -> ()
        | [ value ] -> add_json buffer value
        | value :: rest ->
            add_json buffer value;
            Buffer.add_char buffer ',';
            add_values rest
      in
      add_values values;
      Buffer.add_char buffer ']'
  | Object fields ->
      Buffer.add_char buffer '{';
      let rec add_fields = function
        | [] -> ()
        | [ (name, value) ] ->
            Json_writer.name buffer name;
            add_json buffer value
        | (name, value) :: rest ->
            Json_writer.name buffer name;
            add_json buffer value;
            Buffer.add_char buffer ',';
            add_fields rest
      in
      add_fields fields;
      Buffer.add_char buffer '}'
  | Embedded (type_, value) -> Type.append_json buffer type_ value

let append_json buffer value =
  (* Transactional: a failed append leaves the buffer at its length at entry.
     Only Repr's typed unsupported-projection signal is classified; arbitrary
     callback exceptions propagate so the engine records [Formatting_raised]. *)
  let start = Buffer.length buffer in
  try
    add_json buffer value;
    Ok ()
  with raised -> (
    Buffer.truncate buffer start;
    match raised with
    | Json_writer.Invalid_utf8 -> Error Invalid_utf8
    | Repr.Unsupported_operation _ -> Error Unsupported_value
    | Json_error error -> Error error
    | _ -> raise raised)

let append_json_string buffer value =
  match Json_writer.string buffer value with
  | () -> Ok ()
  | exception Json_writer.Invalid_utf8 -> Error Invalid_utf8

let to_json_string value =
  let buffer = Buffer.create 128 in
  match append_json buffer value with
  | Ok () -> Ok (Buffer.contents buffer)
  | Error _ as error -> error

let rec freeze_into value context ~depth =
  match Snapshot.check_depth context ~depth with
  | Error Snapshot.Limit_exceeded ->
      Snapshot.truncated context ~depth Snapshot.Depth
  | Error _ as error -> error
  | Ok () -> (
      match value with
      | Null | Bool _ | Int _ | Float _ | String _ | Embedded _ ->
          freeze_value context ~depth value
      | List _ | Object _ ->
          Snapshot.localize_apply context ~depth freeze_value value)

and freeze_value context ~depth = function
  | Null -> Snapshot.null context ~depth
  | Bool value -> Snapshot.bool context ~depth value
  | Int value -> Snapshot.int context ~depth value
  | Float value -> (
      match classify_float value with
      | FP_nan | FP_infinite -> Error Snapshot.Conversion_failed
      | FP_normal | FP_subnormal | FP_zero ->
          Snapshot.float context ~depth value)
  | String value -> Snapshot.string context ~depth value
  | List values ->
      Result.bind (Snapshot.List_builder.create context ~depth) (fun builder ->
          let rec collect = function
            | [] -> Snapshot.List_builder.finish builder
            | value :: rest -> (
                match Snapshot.List_builder.prepare builder with
                | Snapshot.Stop -> Snapshot.List_builder.finish builder
                | Snapshot.Ready -> (
                    match freeze_into value context ~depth:(depth + 1) with
                    | Error _ as error -> error
                    | Ok value -> (
                        match Snapshot.List_builder.add builder value with
                        | Snapshot.Stop -> Snapshot.List_builder.finish builder
                        | Snapshot.Ready -> collect rest)))
          in
          collect values)
  | Object fields ->
      Result.bind (Snapshot.Object_builder.create context ~depth)
        (fun builder ->
          let rec collect = function
            | [] -> Snapshot.Object_builder.finish builder
            | (name, value) :: rest -> (
                match Snapshot.Object_builder.prepare builder name with
                | Error _ as error -> error
                | Ok Snapshot.Stop -> Snapshot.Object_builder.finish builder
                | Ok Snapshot.Ready -> (
                    match freeze_into value context ~depth:(depth + 1) with
                    | Error _ as error -> error
                    | Ok value -> (
                        match Snapshot.Object_builder.add builder value with
                        | Snapshot.Stop ->
                            Snapshot.Object_builder.finish builder
                        | Snapshot.Ready -> collect rest)))
          in
          collect fields)
  | Embedded (description, value) ->
      Type.freeze_into description context ~depth value

let freeze ?limits value =
  let context = Snapshot.create_context ?limits () in
  match freeze_into value context ~depth:0 with
  | Ok value -> Ok (Snapshot.seal context value)
  | Error _ as error -> error

let append_frozen_json = Snapshot.append_json
let append_frozen_pretty = Snapshot.append_pretty

let view value =
  let truncation = function
    | Snapshot.Depth -> Depth
    | Snapshot.Object_fields -> Object_fields
    | Snapshot.Collection -> Collection
    | Snapshot.String_bytes -> String_bytes
    | Snapshot.Bytes_length -> Bytes_length
    | Snapshot.Nodes -> Nodes
    | Snapshot.Total_bytes -> Total_bytes
  in
  match Snapshot.view value with
  | `Null -> `Null
  | `Bool value -> `Bool value
  | `Integer (Snapshot.Int value) -> `Integer (`Int value)
  | `Integer (Snapshot.Int32 value) -> `Integer (`Int32 value)
  | `Integer (Snapshot.Int64 value) -> `Integer (`Int64 value)
  | `Integer (Snapshot.Decimal value) -> `Integer (`Decimal value)
  | `Float value -> `Float value
  | `String value -> `String value
  | `Bytes value -> `Bytes value
  | `Truncated reason -> `Truncated (truncation reason)
  | `Truncated_list (values, reason) ->
      `Truncated_list (values, truncation reason)
  | `Truncated_object (fields, reason) ->
      `Truncated_object (fields, truncation reason)
  | `List values -> `List values
  | `Object fields -> `Object fields
  | `Variant (name, polymorphic, payload) ->
      `Variant (name, polymorphic, payload)

let frozen_to_json_string value =
  let buffer = Buffer.create 128 in
  Snapshot.append_json buffer value;
  Buffer.contents buffer
