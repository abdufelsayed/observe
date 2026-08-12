(** Generic pretty projection of an opaque Repr description. The Repr machine is
    encoded once to its JSON representation and decoded into a display tree; the
    tree is then classified and rendered in a single pass. *)

open Pretty

type node =
  | Null
  | Bool of bool
  | Number of string
  | String of string
  | List of node list
  | Object of (string * node) list
  | Record of (string * node) list
  | Variant of { name : string; polymorphic : bool; payload : node option }

let valid_string value =
  if Utf8.is_valid value then value else raise (Error Invalid_utf8)

let number_node value =
  match classify_float value with
  | FP_nan | FP_infinite -> raise (Error Non_finite_float)
  | FP_normal | FP_subnormal | FP_zero ->
      Number (Json_writer.float_to_string value)

let next decoder =
  match Repr.Json.decode decoder with
  | `Lexeme lexeme -> lexeme
  | `Await | `End | `Error _ -> raise (Error Malformed)

let rec decode_value decoder = decode_lexeme decoder (next decoder)

and decode_lexeme decoder = function
  | `Null -> Null
  | `Bool value -> Bool value
  | `Float value -> number_node value
  | `String value -> String (valid_string value)
  | `As -> decode_array decoder []
  | `Os -> decode_object decoder []
  | `Ae | `Oe | `Name _ -> raise (Error Malformed)

and decode_array decoder values =
  match next decoder with
  | `Ae -> List (List.rev values)
  | lexeme -> decode_array decoder (decode_lexeme decoder lexeme :: values)

and decode_object decoder fields =
  match next decoder with
  | `Oe -> Object (List.rev fields)
  | `Name name ->
      let name = valid_string name in
      let value = decode_value decoder in
      decode_object decoder ((name, value) :: fields)
  | `Null | `Bool _ | `Float _ | `String _ | `As | `Ae | `Os ->
      raise (Error Malformed)

let of_repr description value =
  (* Only Repr's own typed unsupported-projection signal is translated. A
     raising user callback inside the Repr traversal keeps its original
     exception so the engine can record [Formatting_raised]. *)
  try
    let encoded = Repr.to_json_string ~minify:true description value in
    let decoder = Repr.Json.decoder (`String encoded) in
    let value = decode_value decoder in
    match Repr.Json.decode decoder with
    | `End -> value
    | `Await | `Error _ | `Lexeme _ -> raise (Error Malformed)
  with Repr.Unsupported_operation _ -> raise (Error Unsupported_value)

let rec plan_node node =
  match node with
  | Null -> Scalar (fun renderer -> null renderer)
  | Bool value -> Scalar (fun renderer -> bool renderer value)
  | Number value -> Scalar (fun renderer -> number renderer value)
  | String value -> Scalar (fun renderer -> string renderer value)
  | List values -> plan_list values
  | Object [] | Record [] -> Scalar (fun renderer -> empty_record renderer)
  | Object fields | Record fields ->
      Node
        (fun renderer placement ->
          let nested = place renderer placement ~scalar:false in
          append_fields renderer fields;
          finish renderer nested)
  | Variant { name; polymorphic; payload = None } ->
      Scalar (fun renderer -> variant renderer ~polymorphic name)
  | Variant { name; polymorphic; payload = Some payload } ->
      Node
        (fun renderer placement ->
          let nested = place renderer placement ~scalar:false in
          let name = if polymorphic then "`" ^ name else name in
          render renderer
            (Constructor { last = true; name })
            (plan_node payload);
          finish renderer nested)

and plan_list values =
  let plans = List.map plan_node values in
  if List.for_all (function Scalar _ -> true | Node _ -> false) plans then
    Scalar
      (fun renderer ->
        list_start renderer;
        append_scalars renderer plans;
        list_end renderer)
  else
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
  | Node _ :: _ -> invalid_arg "Repr_projection.append_scalars: non-scalar node"

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
      render renderer (Field { last = true; name }) (plan_node value)
  | (name, value) :: rest ->
      render renderer (Field { last = false; name }) (plan_node value);
      newline renderer;
      append_fields renderer rest
