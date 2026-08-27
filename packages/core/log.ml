type event =
  | Text of { tag : string; message : string }
  | Structured of { origin : structured_origin }

and structured_origin = Open | Declared of string

type operation_reference = { name : string; id : string }

type operation = {
  reference : operation_reference;
  parent : operation_reference option;
  duration_ns : int64;
}

type annotation = { timestamp : Timestamp.t; level : Level.t; message : string }
type redaction_effect = Removed | Replaced | Masked | Failed_closed

type redaction_location =
  | Structured_value of string
  | Text_message
  | Annotation_message of int

type redaction = {
  redaction_location : redaction_location;
  redaction_effect : redaction_effect;
}

type kind =
  | Point of { correlation : operation_reference option }
  | Wide of { operation : operation; annotations : annotation list }

type t = {
  service : string;
  environment : string option;
  version : string option;
  timestamp : Timestamp.t;
  level : Level.t;
  event : event;
  fields_fragment : Snapshot.fragment;
  kind : kind;
  redactions : redaction list;
}

let service log = log.service
let environment log = log.environment
let version log = log.version
let timestamp log = log.timestamp
let level log = log.level
let event log = log.event
let fields log = Snapshot.complete log.fields_fragment
let kind log = log.kind
let redactions log = log.redactions
let redaction_effect redaction = redaction.redaction_effect
let redaction_location redaction = redaction.redaction_location
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
let completed_structured ~origin = Structured { origin }

module Producer = struct
  type event =
    | Text of { tag : string; message : string; fields : Snapshot.fragment }
    | Structured of { origin : structured_origin; fields : Snapshot.fragment }

  let copy_string value = Bytes.unsafe_to_string (Bytes.of_string value)

  let add_lengths left right =
    if right > max_int - left then max_int else left + right

  let text_valid limits value =
    String.length value <= Log_limits.max_string_bytes limits
    && Snapshot.valid_text value

  let text_length_valid limits value =
    String.length value <= Log_limits.max_string_bytes limits

  let option_valid limits = function
    | None -> true
    | Some value -> text_valid limits value

  let option_length_valid limits = function
    | None -> true
    | Some value -> text_length_valid limits value

  let option_length = Option.fold ~none:0 ~some:String.length

  let reference_valid limits reference =
    text_valid limits reference.name && text_valid limits reference.id

  let reference_length_valid limits reference =
    text_length_valid limits reference.name
    && text_length_valid limits reference.id

  let reference_length reference =
    add_lengths (String.length reference.name) (String.length reference.id)

  let operation_valid limits operation =
    reference_valid limits operation.reference
    && (match operation.parent with
      | None -> true
      | Some parent -> reference_valid limits parent)
    && Int64.compare operation.duration_ns 0L >= 0

  let operation_length_valid limits operation =
    reference_length_valid limits operation.reference
    &&
    match operation.parent with
    | None -> true
    | Some parent -> reference_length_valid limits parent

  let operation_length operation =
    add_lengths
      (reference_length operation.reference)
      (Option.fold ~none:0 ~some:reference_length operation.parent)

  let event_valid limits = function
    | Text { tag; message; fields = _ } ->
        text_valid limits tag && text_valid limits message
    | Structured { origin = Open; fields = _ } -> true
    | Structured { origin = Declared name; fields = _ } ->
        text_valid limits name

  let event_length_valid limits = function
    | Text { tag; message; fields = _ } ->
        text_length_valid limits tag && text_length_valid limits message
    | Structured { origin = Open; fields = _ } -> true
    | Structured { origin = Declared name; fields = _ } ->
        text_length_valid limits name

  let event_length = function
    | Text { tag; message; fields = _ } ->
        add_lengths (String.length tag) (String.length message)
    | Structured { origin = Open; fields = _ } -> 0
    | Structured { origin = Declared name; fields = _ } -> String.length name

  let event_snapshot = function
    | Text { fields; _ } | Structured { fields; _ } -> fields

  let annotation_valid limits annotation = text_valid limits annotation.message

  let annotation_length_valid limits annotation =
    text_length_valid limits annotation.message

  let annotations_length annotations =
    List.fold_left
      (fun length annotation ->
        add_lengths length (String.length annotation.message))
      0 annotations

  let owns_reserved_field =
    Snapshot.root_has_field_matching Log_envelope.is_reserved_field

  let own_event fields = function
    | Text { tag; message; fields = _ } ->
        if (not (Snapshot.is_object fields)) || owns_reserved_field fields then
          Error Snapshot.Conversion_failed
        else
          Ok
            (completed_text ~tag:(copy_string tag)
               ~message:(copy_string message))
    | Structured { origin; fields = _ } ->
        if (not (Snapshot.is_object fields)) || owns_reserved_field fields then
          Error Snapshot.Conversion_failed
        else Ok (completed_structured ~origin)

  let kind_valid limits = function
    | Point { correlation = None } -> true
    | Point { correlation = Some correlation } ->
        reference_valid limits correlation
    | Wide { operation; annotations } ->
        operation_valid limits operation
        &&
        let rec valid = function
          | [] -> true
          | annotation :: rest ->
              annotation_valid limits annotation && valid rest
        in
        valid annotations

  let kind_length_valid limits = function
    | Point { correlation = None } -> true
    | Point { correlation = Some correlation } ->
        reference_length_valid limits correlation
    | Wide { operation; annotations } ->
        operation_length_valid limits operation
        &&
        let rec valid = function
          | [] -> true
          | annotation :: rest ->
              annotation_length_valid limits annotation && valid rest
        in
        valid annotations

  let kind_length = function
    | Point { correlation } ->
        Option.fold ~none:0 ~some:reference_length correlation
    | Wide { operation; annotations } ->
        add_lengths
          (operation_length operation)
          (annotations_length annotations)

  let annotation_count = function
    | Point _ -> 0
    | Wide { annotations; _ } -> List.length annotations

  let redaction_valid limits redaction =
    match redaction.redaction_location with
    | Structured_value path -> text_valid limits path
    | Text_message -> true
    | Annotation_message index -> index >= 0

  let redactions_valid limits redactions =
    List.length redactions <= Log_limits.max_collection_length limits
    && List.for_all (redaction_valid limits) redactions

  let redaction_length_valid limits redaction =
    match redaction.redaction_location with
    | Structured_value path -> text_length_valid limits path
    | Text_message -> true
    | Annotation_message index -> index >= 0

  let redactions_length_valid limits redactions =
    List.length redactions <= Log_limits.max_collection_length limits
    && List.for_all (redaction_length_valid limits) redactions

  let redactions_length redactions =
    List.fold_left
      (fun length redaction ->
        let length = add_lengths length 32 in
        match redaction.redaction_location with
        | Structured_value path -> add_lengths length (String.length path)
        | Text_message | Annotation_message _ -> length)
      0 redactions

  let kind_accepts_event kind event =
    match (kind, event) with
    | Point _, (Text _ | Structured _) | Wide _, Structured _ -> true
    | Wide _, Text _ -> false

  let make ~service ?environment ?version ~timestamp ~level ~kind
      ?(limits = Log_limits.default) ?(redactions = []) event =
    if annotation_count kind > Log_limits.max_collection_length limits then
      Error Snapshot.Limit_exceeded
    else if
      not
        (text_valid limits service
        && option_valid limits environment
        && option_valid limits version
        && kind_valid limits kind
        && redactions_valid limits redactions
        && kind_accepts_event kind event
        && event_valid limits event)
    then
      (* The successful path validates each string once. Only malformed input
         needs the second, length-only pass to preserve the public distinction
         between a configured limit and invalid UTF-8. *)
      if
        text_length_valid limits service
        && option_length_valid limits environment
        && option_length_valid limits version
        && kind_length_valid limits kind
        && redactions_length_valid limits redactions
        && event_length_valid limits event
      then Error Snapshot.Invalid_utf8
      else Error Snapshot.Limit_exceeded
    else
      let string_bytes =
        add_lengths
          (add_lengths
             (add_lengths
                (add_lengths (String.length service)
                   (option_length environment))
                (option_length version))
             (kind_length kind))
          (add_lengths (event_length event) (redactions_length redactions))
      in
      match
        Snapshot.fit_object_extension ~limits (event_snapshot event)
          ~retained_bytes:string_bytes
      with
      | Error _ as error -> error
      | Ok fields_fragment -> (
          let fields_fragment = Snapshot.compact_fragment fields_fragment in
          let fields = Snapshot.complete fields_fragment in
          match own_event fields event with
          | Error _ as error -> error
          | Ok completed_event ->
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
                  event = completed_event;
                  fields_fragment;
                  kind;
                  redactions;
                })

  let operation_reference ~name ~id = { name; id }

  let operation ~name ~id ?parent ~duration_ns () =
    { reference = operation_reference ~name ~id; parent; duration_ns }

  let annotation ~timestamp ~level ~message = { timestamp; level; message }
  let fields_fragment log = log.fields_fragment

  let redaction ~location ~action =
    { redaction_location = location; redaction_effect = action }

  let with_redaction log ~limits ?message ?fields ?annotations ~redactions () =
    let event =
      match (message, log.event) with
      | None, event -> event
      | Some message, Text { tag; message = _ } -> Text { tag; message }
      | Some _, Structured _ -> invalid_arg "structured log has no text message"
    in
    let kind =
      match (annotations, log.kind) with
      | None, kind -> kind
      | Some annotations, Wide { operation; annotations = _ } ->
          Wide { operation; annotations }
      | Some _, Point _ -> invalid_arg "point log has no annotations"
    in
    let fields = Option.value fields ~default:log.fields_fragment in
    let producer_event =
      match event with
      | Text { tag; message } -> Text { tag; message; fields }
      | Structured { origin } -> Structured { origin; fields }
    in
    make ~service:log.service ?environment:log.environment ?version:log.version
      ~timestamp:log.timestamp ~level:log.level ~kind ~limits
      ~redactions:(log.redactions @ redactions)
      producer_event
end
