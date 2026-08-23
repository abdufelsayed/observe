type ('record, 'builder) t = {
  identity : identity;
  name : string;
  builder : 'builder;
  freeze_complete : 'record -> (Snapshot.fragment, Snapshot.error) result;
}

and identity = unit ref

type fragment = (Snapshot.fragment, Snapshot.error) result
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
      {
        identity;
        name;
        builder = builder identity;
        freeze_complete = Type.freeze description;
      }
  | None ->
      invalid_arg
        "Observe.Generated_runtime.record_schema: expected a record description"

let name schema = schema.name
let identity schema = schema.identity
let same_identity left right = left == right
let builder schema = schema.builder
let freeze_complete schema value = schema.freeze_complete value
let fragment description value = Type.freeze description value
let fragment_of_result result = result
let patch_fragment patch = patch.body
let field name fragment = (name, fragment)

let make_identified_patch_fields schema_identity fields =
  let body =
    let rec collect one collected = function
      | [] -> (
          match (one, collected) with
          | None, [] -> Snapshot.object_from_owned []
          | Some (name, value), [] ->
              Snapshot.singleton_object_from_owned name value
          | None, collected -> Snapshot.object_from_owned (List.rev collected)
          | Some _, _ -> assert false)
      | (_, Error error) :: _ -> Error error
      | (name, Ok value) :: rest -> (
          match (one, collected) with
          | None, [] -> collect (Some (name, value)) [] rest
          | Some field, [] -> collect None [ (name, value); field ] rest
          | None, collected -> collect None ((name, value) :: collected) rest
          | Some _, _ -> assert false)
    in
    collect None [] fields
  in
  { schema_identity; body; has_error = false }

(* Keep the option-list bridge allocation-compatible with previously generated
   code. Current generators use [make_identified_patch_fields] and never allocate
   slots for absent fields. *)
let make_identified_patch schema_identity fields =
  let body =
    let rec collect one collected = function
      | [] -> (
          match (one, collected) with
          | None, [] -> Snapshot.object_from_owned []
          | Some (name, value), [] ->
              Snapshot.singleton_object_from_owned name value
          | None, collected -> Snapshot.object_from_owned (List.rev collected)
          | Some _, _ -> assert false)
      | None :: rest -> collect one collected rest
      | Some (_, Error error) :: _ -> Error error
      | Some (name, Ok value) :: rest -> (
          match (one, collected) with
          | None, [] -> collect (Some (name, value)) [] rest
          | Some field, [] -> collect None [ (name, value); field ] rest
          | None, collected -> collect None ((name, value) :: collected) rest
          | Some _, _ -> assert false)
    in
    collect None [] fields
  in
  { schema_identity; body; has_error = false }

let make_identified_error_patch schema_identity body =
  { schema_identity; body; has_error = true }

let make_patch schema fields = make_identified_patch schema.identity fields

let make_patch_fields schema fields =
  make_identified_patch_fields schema.identity fields

let combine_identified_patches schema_identity patches =
  let rec combine accumulator has_error = function
    | [] ->
        {
          schema_identity;
          body = Ok (Snapshot.Object_accumulator.as_fragment accumulator);
          has_error;
        }
    | patch :: rest
      when not (same_identity patch.schema_identity schema_identity) ->
        {
          schema_identity;
          body = Error Snapshot.Unsupported;
          has_error = has_error || patch.has_error;
        }
    | { body = Error error; has_error = patch_error; _ } :: _ ->
        {
          schema_identity;
          body = Error error;
          has_error = has_error || patch_error;
        }
    | { body = Ok patch; has_error = patch_error; _ } :: rest -> (
        match Snapshot.Object_accumulator.merge_disjoint accumulator patch with
        | Error error ->
            {
              schema_identity;
              body = Error error;
              has_error = has_error || patch_error;
            }
        | Ok accumulator -> combine accumulator (has_error || patch_error) rest)
  in
  combine Snapshot.Object_accumulator.empty false patches

let has_error patch = patch.has_error

let body schema patch =
  if same_identity schema.identity patch.schema_identity then patch.body
  else Error Snapshot.Unsupported
