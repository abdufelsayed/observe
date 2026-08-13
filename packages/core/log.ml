type body =
  | Text of { tag : string; message : string }
  | Untyped of Value.t
  | Typed : 'a Type.t * 'a -> body

type t = {
  service : string;
  environment : string option;
  version : string option;
  timestamp : Timestamp.t;
  level : Level.t;
  body : body;
}

let service log = log.service
let environment log = log.environment
let version log = log.version
let timestamp log = log.timestamp
let level log = log.level
let body log = log.body

module Producer = struct
  let make ~service ?environment ?version ~timestamp ~level body =
    { service; environment; version; timestamp; level; body }
end
