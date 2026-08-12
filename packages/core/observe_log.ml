type payload =
  | Text of { tag : string; message : string }
  | Free of Observe_value.t
  | Structured : 'a Observe_type.t * 'a -> payload

type t = {
  service : string;
  environment : string option;
  version : string option;
  instant : Observe_instant.t;
  level : Observe_level.t;
  payload : payload;
}

let service log = log.service
let environment log = log.environment
let version log = log.version
let instant log = log.instant
let level log = log.level
let payload log = log.payload

module Producer = struct
  let make ~service ?environment ?version ~instant ~level payload =
    { service; environment; version; instant; level; payload }
end
