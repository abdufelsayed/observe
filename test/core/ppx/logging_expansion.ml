type event = { user_id : int } [@@deriving observe]

let level = Observe.Level.Error

let () =
  [%observe.debug text ~tag:"router" "matched %s" "/checkout"];
  [%observe.info untyped { action = "login" }];
  [%observe.warn typed ~using:event_schema { user_id = 42 }];
  [%observe.error text ~tag:"payment" "failed"];
  Observe.Logs.log ~level (fun m -> m.typed ~using:event_schema { user_id = 7 })
