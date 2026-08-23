type event =
  | Text of { tag : string; message : string }
  | Structured of { origin : structured_origin; value : Value.frozen }

and structured_origin = Open | Declared of string

type kind = Point | Wide
type operation_reference = { name : string; id : string }

type operation = {
  reference : operation_reference;
  parent : operation_reference option;
  duration_ns : int64;
}

type annotation = { timestamp : Timestamp.t; level : Level.t; message : string }

type t = {
  service : string;
  environment : string option;
  version : string option;
  timestamp : Timestamp.t;
  level : Level.t;
  event : event;
  correlation : operation_reference option;
  operation : operation option;
  annotations : annotation list;
}

let service log = log.service
let environment log = log.environment
let version log = log.version
let timestamp log = log.timestamp
let level log = log.level
let event log = log.event
let kind log = match log.operation with None -> Point | Some _ -> Wide
let operation log = log.operation
let correlation log = log.correlation
let annotations log = log.annotations
let operation_reference_name reference = reference.name
let operation_reference_id reference = reference.id
let operation_name operation = operation.reference.name
let operation_id operation = operation.reference.id
let operation_parent operation = operation.parent
let operation_duration_ns operation = operation.duration_ns
let annotation_timestamp (annotation : annotation) = annotation.timestamp
let annotation_level (annotation : annotation) = annotation.level
let annotation_message (annotation : annotation) = annotation.message
let completed_text ~tag ~message = Text { tag; message }
let completed_structured ~origin ~value = Structured { origin; value }

module Producer = struct
  type event =
    | Text of { tag : string; message : string }
    | Structured of { origin : structured_origin; value : Snapshot.fragment }

  let copy_string value = Bytes.unsafe_to_string (Bytes.of_string value)
  let option_valid = Option.fold ~none:true ~some:Snapshot.valid_text
  let option_length = Option.fold ~none:0 ~some:String.length

  let reference_valid reference =
    Snapshot.valid_text reference.name && Snapshot.valid_text reference.id

  let reference_length reference =
    String.length reference.name + String.length reference.id

  let operation_valid = function
    | None -> true
    | Some operation ->
        reference_valid operation.reference
        && Option.fold ~none:true ~some:reference_valid operation.parent

  let operation_length = function
    | None -> 0
    | Some operation ->
        reference_length operation.reference
        + Option.fold ~none:0 ~some:reference_length operation.parent

  let event_valid = function
    | Text { tag; message } ->
        Snapshot.valid_text tag && Snapshot.valid_text message
    | Structured { origin = Open; _ } -> true
    | Structured { origin = Declared name; _ } -> Snapshot.valid_text name

  let event_length = function
    | Text { tag; message } -> String.length tag + String.length message
    | Structured { origin = Open; _ } -> 0
    | Structured { origin = Declared name; _ } -> String.length name

  let event_snapshot = function
    | Text _ -> None
    | Structured { value; _ } -> Some value

  let annotation_valid annotation = Snapshot.valid_text annotation.message

  let annotations_length annotations =
    List.fold_left
      (fun length annotation -> length + String.length annotation.message)
      0 annotations

  let reserved_field = function
    | "service" | "environment" | "version" | "timestamp" | "level"
    | "operation" | "operation_id" | "parent_operation" | "parent_operation_id"
    | "duration_ms" | "tag" | "message" | "logs" ->
        true
    | _ -> false

  let owns_reserved_field = Snapshot.root_has_field_matching reserved_field

  let own_event = function
    | Text { tag; message } ->
        Ok
          (completed_text ~tag:(copy_string tag) ~message:(copy_string message))
    | Structured { origin; value } ->
        let value = Snapshot.complete value in
        if (not (Snapshot.is_object value)) || owns_reserved_field value then
          Error Snapshot.Conversion_failed
        else Ok (completed_structured ~origin ~value)

  let make ~service ?environment ?version ~timestamp ~level ?correlation
      ?operation ?(annotations = []) event =
    if List.length annotations > Snapshot.width_limit then
      Error Snapshot.Limit_exceeded
    else if
      not
        (Snapshot.valid_text service
        && option_valid environment
        && option_valid version
        && Option.fold ~none:true ~some:reference_valid correlation
        && operation_valid operation
        && List.for_all annotation_valid annotations
        && event_valid event)
    then Error Snapshot.Invalid_utf8
    else
      let string_bytes =
        String.length service
        + option_length environment
        + option_length version
        + Option.fold ~none:0 ~some:reference_length correlation
        + operation_length operation
        + annotations_length annotations
        + event_length event
      in
      match
        Snapshot.validate_extension (event_snapshot event) ~nodes:0
          ~string_bytes ~byte_bytes:0 ~retained_bytes:string_bytes
      with
      | Error _ as error -> error
      | Ok () -> (
          match own_event event with
          | Error _ as error -> error
          | Ok event ->
              Ok
                {
                  (* Configuration, operation references, and annotations are
                     already package-owned before they reach this boundary.
                     Only author-provided text is copied by [own_event]. *)
                  service;
                  environment;
                  version;
                  timestamp;
                  level;
                  event;
                  correlation;
                  operation;
                  annotations;
                })

  let operation_reference ~name ~id = { name; id }

  let operation ~name ~id ?parent ~duration_ns () =
    { reference = operation_reference ~name ~id; parent; duration_ns }

  let annotation ~timestamp ~level ~message = { timestamp; level; message }
end
