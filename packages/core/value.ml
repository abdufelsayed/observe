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

let freeze value =
  let context = Snapshot.create_context () in
  let rec freeze_at ~depth value =
    match Snapshot.check_depth ~depth with
    | Error _ as error -> error
    | Ok () -> (
        match value with
        | Null -> Snapshot.null context ~depth
        | Bool value -> Snapshot.bool context ~depth value
        | Int value ->
            let buffer = Buffer.create 24 in
            Json_writer.int buffer value;
            Snapshot.integer context ~depth (Buffer.contents buffer)
        | Float value -> (
            match classify_float value with
            | FP_nan | FP_infinite -> Error Snapshot.Conversion_failed
            | FP_normal | FP_subnormal | FP_zero ->
                Snapshot.float context ~depth value)
        | String value -> Snapshot.string context ~depth value
        | List values ->
            let rec collect count frozen = function
              | [] -> Snapshot.list context ~depth (List.rev frozen)
              | _ when count >= Snapshot.width_limit ->
                  Error Snapshot.Limit_exceeded
              | value :: rest -> (
                  match freeze_at ~depth:(depth + 1) value with
                  | Error _ as error -> error
                  | Ok value -> collect (count + 1) (value :: frozen) rest)
            in
            collect 0 [] values
        | Object fields ->
            let rec collect count frozen = function
              | [] -> Snapshot.object_ context ~depth (List.rev frozen)
              | _ when count >= Snapshot.width_limit ->
                  Error Snapshot.Limit_exceeded
              | (name, value) :: rest -> (
                  match freeze_at ~depth:(depth + 1) value with
                  | Error _ as error -> error
                  | Ok value ->
                      collect (count + 1) ((name, value) :: frozen) rest)
            in
            collect 0 [] fields
        | Embedded (description, value) ->
            Type.freeze_into description context ~depth value)
  in
  freeze_at ~depth:0 value

let append_frozen_json = Snapshot.append_json
let append_frozen_pretty = Snapshot.append_pretty

let frozen_to_json_string value =
  let buffer = Buffer.create 128 in
  Snapshot.append_json buffer value;
  Buffer.contents buffer
