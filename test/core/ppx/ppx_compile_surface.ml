type event = { request_id : string; attempts : int } [@@deriving observe]

let description : event Observe.Type.t = event_t

let value =
  [%observe.value
    {
      request_id = "req-1";
      attempts = 2;
      typed = [%observe.value.embed Observe.Type.int, 7];
    }]

let text_log () =
  [%observe.info text ~tag:"request" "received request %s" "req-1"]

let untyped_log () = [%observe.warn untyped { action = "retry"; attempts = 2 }]

let typed_log () =
  [%observe.error typed event_schema { request_id = "req-1"; attempts = 2 }]

let dynamic_log level () =
  [%observe.emit
    level, typed event_schema { request_id = "req-2"; attempts = 0 }]

let () =
  ignore description;
  ignore value;
  ignore text_log;
  ignore untyped_log;
  ignore typed_log;
  ignore dynamic_log
