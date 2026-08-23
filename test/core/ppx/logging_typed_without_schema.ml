type event = { value : int } [@@deriving observe]

let _ = [%observe.info typed event_schema { value = 1 }]
