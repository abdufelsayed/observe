type event = { service : string } [@@deriving observe]

let _ = [%observe.info typed ~using:event_schema { service = "consumer-value" }]
