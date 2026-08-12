type event =
  | User_login of { user_id : int; method_ : string }
  | Cache_miss of { key : string }
[@@deriving observe]

val sample : event
