type event = User_login of int [@@deriving observe]

let level = Observe.Level.Error

let () =
  [%observe.debug text ~tag:"router" "matched %s" "/checkout"];
  [%observe.info untyped [%observe.value { action = "login" }]];
  [%observe.warn typed event_t (User_login 42)];
  [%observe.error text ~tag:"payment" "failed"];
  [%observe.emit level, typed event_t (User_login 7)]
