type ('record, 'builder) t = {
  name : string;
  builder : 'builder;
  freeze_complete : 'record -> (Snapshot.fragment, Snapshot.error) result;
}

type fragment = (Snapshot.fragment, Snapshot.error) result
type field = string * fragment
type 'record patch = { schema_name : string; body : fragment; has_error : bool }

let same_schema left right = left == right || String.equal left right

let record ?name ~builder description =
  match Type.record_name description with
  | Some record_name ->
      let name =
        Option.value name ~default:record_name |> fun value ->
        Bytes.unsafe_to_string (Bytes.of_string value)
      in
      {
        name;
        builder = builder name;
        freeze_complete = Type.freeze description;
      }
  | None ->
      invalid_arg
        "Observe.Generated_runtime.record_schema: expected a record description"

let name schema = schema.name
let builder schema = schema.builder
let freeze_complete schema value = schema.freeze_complete value
let fragment description value = Type.freeze description value
let fragment_of_result result = result
let patch_fragment patch = patch.body
let field name fragment = (name, fragment)

let make_named_patch_fields schema_name fields =
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
  { schema_name; body; has_error = false }

(* Keep the option-list bridge allocation-compatible with previously generated
   code. Current generators use [make_named_patch_fields] and never allocate
   slots for absent fields. *)
let make_named_patch schema_name fields =
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
  { schema_name; body; has_error = false }

let make_named_error_patch schema_name body =
  { schema_name; body; has_error = true }

let make_patch schema fields = make_named_patch schema.name fields
let make_patch_fields schema fields = make_named_patch_fields schema.name fields

let combine_named_patches schema_name patches =
  let rec combine accumulator has_error = function
    | [] ->
        {
          schema_name;
          body = Ok (Snapshot.Object_accumulator.as_fragment accumulator);
          has_error;
        }
    | patch :: rest when not (same_schema patch.schema_name schema_name) ->
        {
          schema_name;
          body = Error Snapshot.Unsupported;
          has_error = has_error || patch.has_error;
        }
    | { body = Error error; has_error = patch_error; _ } :: _ ->
        {
          schema_name;
          body = Error error;
          has_error = has_error || patch_error;
        }
    | { body = Ok patch; has_error = patch_error; _ } :: rest -> (
        match Snapshot.Object_accumulator.merge accumulator patch with
        | Error error ->
            {
              schema_name;
              body = Error error;
              has_error = has_error || patch_error;
            }
        | Ok accumulator -> combine accumulator (has_error || patch_error) rest)
  in
  combine Snapshot.Object_accumulator.empty false patches

let has_error patch = patch.has_error

let body schema patch =
  if same_schema schema.name patch.schema_name then patch.body
  else Error Snapshot.Unsupported
