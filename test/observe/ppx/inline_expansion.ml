type event = User_login of { user_id : int; method_ : string } | Idle
[@@deriving observe]
