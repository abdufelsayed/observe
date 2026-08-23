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
  [%observe.error
    typed ~using:event_schema { request_id = "req-1"; attempts = 2 }]

let reordered_labels exn backtrace () =
  [%observe.info
    typed { request_id = "req-2"; attempts = 3 } ~using:event_schema];
  [%observe.error error exn ~backtrace ~using:Observe.Error.exn]

let dynamic_log level () =
  Observe.Logs.log ~level (fun m ->
      m.typed ~using:event_schema { request_id = "req-2"; attempts = 0 })

let () =
  ignore description;
  ignore value;
  ignore text_log;
  ignore untyped_log;
  ignore typed_log;
  ignore reordered_labels;
  ignore dynamic_log
