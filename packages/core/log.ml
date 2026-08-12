type payload =
  | Text of { tag : string; message : string }
  | Free of Value.t
  | Structured : 'a Type.t * 'a -> payload

type t = {
  service : string;
  environment : string option;
  version : string option;
  timestamp : Timestamp.t;
  level : Level.t;
  payload : payload;
}

let service log = log.service
let environment log = log.environment
let version log = log.version
let timestamp log = log.timestamp
let level log = log.level
let payload log = log.payload

module Producer = struct
  let make ~service ?environment ?version ~timestamp ~level payload =
    { service; environment; version; timestamp; level; payload }
end
