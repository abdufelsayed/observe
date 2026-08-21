type ('record, 'builder) t = {
  name : string;
  builder : 'builder;
  freeze_complete : 'record -> (Snapshot.t, Snapshot.error) result;
}

type fragment = (Snapshot.t, Snapshot.error) result
type field = string * fragment
type 'record patch = { schema_name : string; body : fragment; has_error : bool }

let record ?name ~builder description =
  match Type.record_name description with
  | Some record_name ->
      let name = Option.value name ~default:record_name in
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

let make_named_patch schema_name fields =
  let body =
    let rec collect collected = function
      | [] -> Snapshot.refreeze (Snapshot.Object (List.rev collected))
      | None :: rest -> collect collected rest
      | Some (_, Error error) :: _ -> Error error
      | Some (name, Ok value) :: rest ->
          collect ((name, value) :: collected) rest
    in
    collect [] fields
  in
  { schema_name; body; has_error = false }

let make_named_error_patch schema_name body =
  { schema_name; body; has_error = true }

let make_patch schema fields = make_named_patch schema.name fields

let combine_named_patches schema_name patches =
  let rec combine body has_error = function
    | [] -> { schema_name; body; has_error }
    | patch :: rest when not (String.equal patch.schema_name schema_name) ->
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
        match body with
        | Error error -> { schema_name; body = Error error; has_error }
        | Ok body ->
            combine
              (Snapshot.merge_object body patch)
              (has_error || patch_error) rest)
  in
  combine (Ok (Snapshot.Object [])) false patches

let has_error patch = patch.has_error

let body schema patch =
  if String.equal schema.name patch.schema_name then patch.body
  else Error Snapshot.Unsupported
