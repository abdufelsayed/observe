type payload = { request_id : string; attempts : int } [@@deriving observe]
type event = Request of payload | Idle [@@deriving observe]

let typed =
 fun (m : Observe.Logs.builder) ->
  m.typed event_t (Request { request_id = "r1"; attempts = 2 })

let untyped =
 fun (m : Observe.Logs.builder) ->
  m.untyped
    [%observe.value
      {
        request_id = "r1";
        attempts = 2;
        typed = [%observe.value.embed Observe.Type.int, 7];
      }]

let text_log () = [%observe.info text ~tag:"consumer" "request %s" "r1"]

let untyped_log () =
  [%observe.warn untyped [%observe.value { request_id = "r1"; attempts = 2 }]]

let typed_log () =
  [%observe.error typed event_t (Request { request_id = "r1"; attempts = 2 })]

let _ = (typed, untyped, text_log, untyped_log, typed_log)
