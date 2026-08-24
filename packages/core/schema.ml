type ('record, 'builder) t = {
  identity : identity;
  name : string;
  builder : 'builder;
  freeze_complete : 'record -> materializer;
}

and identity = unit ref

and materializer =
  Snapshot.context -> depth:int -> (Snapshot.value, Snapshot.error) result

type 'record patch_builder = { schema_identity : identity }
type fragment = materializer
type field = string * fragment

type 'record patch = {
  schema_identity : identity;
  body : fragment;
  has_error : bool;
}

let record ?name ~builder description =
  match Type.record_name description with
  | Some record_name ->
      let name =
        Option.value name ~default:record_name |> fun value ->
        Bytes.unsafe_to_string (Bytes.of_string value)
      in
      let identity = ref () in
      let patch_builder = { schema_identity = identity } in
      {
        identity;
        name;
        builder = builder patch_builder;
        freeze_complete =
          (fun value context ~depth ->
            Type.freeze_into description context ~depth value);
      }
  | None -> invalid_arg "Observe.Schema.record: expected a record description"

let name schema = schema.name
let identity schema = schema.identity
let same_identity left right = left == right
let builder schema = schema.builder

let freeze_complete schema value context ~depth =
  schema.freeze_complete value context ~depth

let fragment description value context ~depth =
  Type.freeze_into description context ~depth value

let fragment_of_value value context ~depth =
  Value.freeze_into value context ~depth

let materialize fragment context ~depth = fragment context ~depth
let patch_fragment patch = patch.body
let field name fragment = (name, fragment)

let make_identified_patch_fields (patch_builder : 'record patch_builder) fields
    =
  let body context ~depth =
    let rec collect collected = function
      | [] -> Snapshot.object_ context ~depth collected
      | (name, materialize) :: rest -> (
          match materialize context ~depth:(depth + 1) with
          | Error _ as error -> error
          | Ok value -> collect ((name, value) :: collected) rest)
    in
    collect [] fields
  in
  { schema_identity = patch_builder.schema_identity; body; has_error = false }

(* Keep the option-list bridge allocation-compatible with previously generated
   code. Current generators use [make_identified_patch_fields] and never allocate
   slots for absent fields. *)
let make_identified_patch (patch_builder : 'record patch_builder) fields =
  let fields = List.filter_map Fun.id fields in
  let body context ~depth =
    let rec collect collected = function
      | [] -> Snapshot.object_ context ~depth collected
      | (name, materialize) :: rest -> (
          match materialize context ~depth:(depth + 1) with
          | Error _ as error -> error
          | Ok value -> collect ((name, value) :: collected) rest)
    in
    collect [] fields
  in
  { schema_identity = patch_builder.schema_identity; body; has_error = false }

let make_identified_error_patch (patch_builder : 'record patch_builder) body =
  { schema_identity = patch_builder.schema_identity; body; has_error = true }

let schema_patch_builder schema : 'record patch_builder =
  { schema_identity = schema.identity }

let make_patch schema fields =
  make_identified_patch (schema_patch_builder schema) fields

let make_patch_fields schema fields =
  make_identified_patch_fields (schema_patch_builder schema) fields

let combine_identified_patches (patch_builder : 'record patch_builder) patches =
  let schema_identity = patch_builder.schema_identity in
  let valid_identity =
    List.for_all
      (fun patch -> same_identity patch.schema_identity schema_identity)
      patches
  in
  let has_error = List.exists (fun patch -> patch.has_error) patches in
  let body context ~depth =
    if not valid_identity then Error Snapshot.Unsupported
    else
      let rec combine accumulator = function
        | [] -> Ok (Snapshot.Object_accumulator.as_value accumulator)
        | patch :: rest -> (
            match patch.body context ~depth with
            | Error _ as error -> error
            | Ok value -> (
                let fragment = Snapshot.seal context value in
                match
                  Snapshot.Object_accumulator.merge_disjoint accumulator
                    fragment
                with
                | Error _ as error -> error
                | Ok accumulator -> combine accumulator rest))
      in
      combine Snapshot.Object_accumulator.empty patches
  in
  { schema_identity; body; has_error }

let has_error patch = patch.has_error

let body schema patch =
  if same_identity schema.identity patch.schema_identity then patch.body
  else fun _ ~depth:_ -> Error Snapshot.Unsupported

let field_patch patch_builder name description value =
  make_identified_patch_fields patch_builder
    [ field name (fragment description value) ]

let nested_patch patch_builder name ~using patch =
  make_identified_patch_fields patch_builder [ field name (body using patch) ]

let combine = combine_identified_patches
