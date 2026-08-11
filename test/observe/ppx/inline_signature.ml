type event =
  | User_login of { user_id : int; method_ : string }
  | Cache_miss of { key : string }
[@@deriving observe]

let sample = User_login { user_id = 42; method_ = "oauth" }

let () =
  ignore event_t;
  ignore sample
