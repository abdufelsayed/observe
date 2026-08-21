type small = {
  action : string;
  user_id : int;
  login_method : string;
  remembered : bool;
  provider : string;
}
[@@deriving observe]

type user = { id : int; plan : string option; roles : string list }
[@@deriving observe]

type oauth = { provider : string; scopes : string list } [@@deriving observe]
type authentication = Password | Oauth of oauth [@@deriving observe]
type denial = { reason : string; retryable : bool } [@@deriving observe]
type access = Granted | Denied of denial [@@deriving observe]

type nested = {
  action : string;
  user : user;
  authentication : authentication;
  access : access;
  remembered : bool;
  device_id : string option;
}
[@@deriving observe]

val small : small
val small_untyped : unit -> Observe.Value.t
val nested : nested
val nested_untyped : unit -> Observe.Value.t
