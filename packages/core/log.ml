type body =
  | Text of { tag : string; message : string }
  | Structured of { origin : structured_origin; value : Value.frozen }

and structured_origin = Open | Declared of string

type kind = Point | Wide

type operation = {
  name : string;
  id : string;
  parent_id : string option;
  duration_ns : int64;
}

type t = {
  service : string;
  environment : string option;
  version : string option;
  timestamp : Timestamp.t;
  level : Level.t;
  body : body;
  correlation_id : string option;
  operation : operation option;
}

let service log = log.service
let environment log = log.environment
let version log = log.version
let timestamp log = log.timestamp
let level log = log.level
let body log = log.body
let kind log = match log.operation with None -> Point | Some _ -> Wide
let operation log = log.operation
let correlation_id log = log.correlation_id
let operation_name operation = operation.name
let operation_id operation = operation.id
let operation_parent_id operation = operation.parent_id
let operation_duration_ns operation = operation.duration_ns
let completed_text ~tag ~message = Text { tag; message }
let completed_structured ~origin ~value = Structured { origin; value }

module Producer = struct
  type body =
    | Text of { tag : string; message : string }
    | Structured of { origin : structured_origin; value : Snapshot.fragment }

  let copy_string value = Bytes.unsafe_to_string (Bytes.of_string value)
  let option_valid = Option.fold ~none:true ~some:Snapshot.valid_text
  let option_length = Option.fold ~none:0 ~some:String.length

  let operation_valid = function
    | None -> true
    | Some operation ->
        Snapshot.valid_text operation.name
        && Snapshot.valid_text operation.id
        && option_valid operation.parent_id

  let operation_length = function
    | None -> 0
    | Some operation ->
        String.length operation.name
        + String.length operation.id
        + option_length operation.parent_id

  let body_valid = function
    | Text { tag; message } ->
        Snapshot.valid_text tag && Snapshot.valid_text message
    | Structured { origin = Open; _ } -> true
    | Structured { origin = Declared name; _ } -> Snapshot.valid_text name

  let body_length = function
    | Text { tag; message } -> String.length tag + String.length message
    | Structured { origin = Open; _ } -> 0
    | Structured { origin = Declared name; _ } -> String.length name

  let body_snapshot = function
    | Text _ -> None
    | Structured { value; _ } -> Some value

  let own_body = function
    | Text { tag; message } -> completed_text ~tag:(copy_string tag) ~message
    | Structured { origin; value } ->
        completed_structured ~origin ~value:(Snapshot.complete value)

  let make ~service ?environment ?version ~timestamp ~level ?correlation_id
      ?operation body =
    if
      not
        (Snapshot.valid_text service
        && option_valid environment
        && option_valid version
        && option_valid correlation_id
        && operation_valid operation
        && body_valid body)
    then Error Snapshot.Invalid_utf8
    else
      let string_bytes =
        String.length service
        + option_length environment
        + option_length version
        + option_length correlation_id
        + operation_length operation
        + body_length body
      in
      match
        Snapshot.validate_extension (body_snapshot body) ~nodes:0 ~string_bytes
          ~byte_bytes:0 ~retained_bytes:string_bytes
      with
      | Error _ as error -> error
      | Ok () ->
          Ok
            {
              service;
              environment;
              version;
              timestamp;
              level;
              body = own_body body;
              correlation_id = Option.map copy_string correlation_id;
              operation;
            }

  let operation ~name ~id ?parent_id ~duration_ns () =
    { name; id; parent_id; duration_ns }
end
