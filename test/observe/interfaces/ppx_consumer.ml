type payload = { request_id : string; attempts : int } [@@deriving observe]
type event = Request of payload | Idle [@@deriving observe]

let typed =
  Observe.Logs.structured event_t (Request { request_id = "r1"; attempts = 2 })

let free =
  Observe.Logs.free
    [%observe.value
      {
        request_id = "r1";
        attempts = 2;
        typed = [%observe.value.embed Observe.Type.int, 7];
      }]

let _ = (typed, free)
