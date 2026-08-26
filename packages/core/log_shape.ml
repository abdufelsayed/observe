type node =
  | Scalar
  | String
  | Opaque
  | Unaddressable
  | Record of (string * t) array
  | Variant of (string * t option) array
  | Tuple of t array
  | Collection of t
  | Option of t
  | Link of t

and t = node ref

type step = Field of string | Index of int | Case of string
type lookup = Known of t | Empty_case | Missing | Opaque | Unaddressable

let scalar : t = ref Scalar
let string : t = ref String
let opaque : t = ref (Opaque : node)
let unaddressable : t = ref (Unaddressable : node)
let own_name value = Bytes.unsafe_to_string (Bytes.of_string value)

let record fields =
  let fields = List.map (fun (name, shape) -> (own_name name, shape)) fields in
  ref (Record (Array.of_list fields))

let variant cases =
  let cases = List.map (fun (name, shape) -> (own_name name, shape)) cases in
  ref (Variant (Array.of_list cases))

let tuple positions = ref (Tuple (Array.of_list positions))
let collection element = ref (Collection element)
let option value = ref (Option value)

type knot = t

let knot () : knot = ref (Opaque : node)
let knot_shape knot = knot

let tie (knot : knot) shape =
  match !knot with
  | Opaque -> knot := Link shape
  | Link _ -> ()
  | Scalar | String | Unaddressable | Record _ | Variant _ | Tuple _
  | Collection _ | Option _ ->
      invalid_arg "Observe.Log_shape.tie: knot already initialized"

let find_field name fields =
  let rec loop index =
    if index = Array.length fields then None
    else
      let candidate, shape = fields.(index) in
      if String.equal name candidate then Some shape else loop (index + 1)
  in
  loop 0

let find_case name cases =
  let rec loop index =
    if index = Array.length cases then None
    else
      let candidate, shape = cases.(index) in
      if String.equal name candidate then Some shape else loop (index + 1)
  in
  loop 0

(* Transparent wrappers are followed without consuming a path component.  A
   recursive description such as [mu (fun self -> option self)] therefore
   cannot make lookup diverge: [seen] records each link followed during one
   lookup and an already-seen node means that the remaining shape is unknown. *)
let rec transparent seen shape =
  if List.exists (fun seen_shape -> seen_shape == shape) seen then None
  else
    match !shape with
    | Link next -> transparent (shape :: seen) next
    | Option next -> transparent (shape :: seen) next
    | node -> Some (shape, node)

let lookup shape steps =
  let rec descend shape steps =
    match transparent [] shape with
    | None -> Opaque
    | Some (shape, node) -> (
        match steps with
        | [] -> Known shape
        | step :: rest -> (
            match (node, step) with
            | Record fields, Field name -> (
                match find_field name fields with
                | None -> Missing
                | Some child -> descend child rest)
            | Tuple positions, Index index ->
                if index < 0 || index >= Array.length positions then Missing
                else descend positions.(index) rest
            | Collection element, Index index ->
                if index < 0 then Missing else descend element rest
            | Variant cases, Case name -> (
                match find_case name cases with
                | None -> Missing
                | Some None -> if rest = [] then Empty_case else Missing
                | Some (Some child) -> descend child rest)
            | Opaque, _ -> Opaque
            | Unaddressable, _ -> Unaddressable
            | ( (Scalar | String | Record _ | Tuple _ | Collection _ | Variant _),
                _ ) ->
                Missing
            | (Option _ | Link _), _ -> Opaque))
  in
  descend shape steps

let accepts_string_mask shape =
  match transparent [] shape with Some (_, String) -> true | _ -> false
