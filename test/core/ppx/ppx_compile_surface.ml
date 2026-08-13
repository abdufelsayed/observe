type event = Request of { request_id : string; attempts : int } | Idle
[@@deriving observe]

let description : event Observe.Type.t = event_t

let value =
  [%observe.value
    {
      request_id = "req-1";
      attempts = 2;
      typed = [%observe.value.embed Observe.Type.int, 7];
    }]

let () =
  ignore description;
  ignore value
