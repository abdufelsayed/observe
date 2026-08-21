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
let operation_name operation = operation.name
let operation_id operation = operation.id
let operation_parent_id operation = operation.parent_id
let operation_duration_ns operation = operation.duration_ns

module Producer = struct
  let copy_optional context = function
    | None -> Ok None
    | Some value ->
        Result.map Option.some (Snapshot.copy_text context ~depth:0 value)

  let copy_operation context = function
    | None -> Ok None
    | Some operation -> (
        match Snapshot.copy_text context ~depth:0 operation.name with
        | Error _ as error -> error
        | Ok name -> (
            match Snapshot.copy_text context ~depth:0 operation.id with
            | Error _ as error -> error
            | Ok id ->
                Result.map
                  (fun parent_id -> Some { operation with name; id; parent_id })
                  (copy_optional context operation.parent_id)))

  let copy_body context = function
    | Text { tag; message } -> (
        match Snapshot.copy_text context ~depth:0 tag with
        | Error _ as error -> error
        | Ok tag ->
            Result.map
              (fun message -> Text { tag; message })
              (Snapshot.copy_text context ~depth:0 message))
    | Structured { origin; value } -> (
        let origin =
          match origin with
          | Open -> Ok Open
          | Declared name ->
              Result.map
                (fun name -> Declared name)
                (Snapshot.copy_text context ~depth:0 name)
        in
        match origin with
        | Error _ as error -> error
        | Ok origin ->
            Result.map
              (fun value -> Structured { origin; value })
              (Snapshot.refreeze_into context value))

  let make ~service ?environment ?version ~timestamp ~level ?operation body =
    let context = Snapshot.create_context () in
    match Snapshot.copy_text context ~depth:0 service with
    | Error _ as error -> error
    | Ok service -> (
        match copy_optional context environment with
        | Error _ as error -> error
        | Ok environment -> (
            match copy_optional context version with
            | Error _ as error -> error
            | Ok version -> (
                match copy_operation context operation with
                | Error _ as error -> error
                | Ok operation ->
                    Result.map
                      (fun body ->
                        {
                          service;
                          environment;
                          version;
                          timestamp;
                          level;
                          body;
                          operation;
                        })
                      (copy_body context body))))

  let operation ~name ~id ?parent_id ~duration_ns () =
    { name; id; parent_id; duration_ns }
end
