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

let small =
  {
    action = "user_login";
    user_id = 42;
    login_method = "oauth";
    remembered = true;
    provider = "github";
  }

let nested =
  {
    action = "user_login";
    user = { id = 42; plan = Some "pro"; roles = [ "admin"; "billing" ] };
    authentication =
      Oauth { provider = "github"; scopes = [ "openid"; "profile" ] };
    access = Granted;
    remembered = true;
    device_id = Some "device_7f3a";
  }

let small_untyped () =
  Observe.Value.object_
    [
      ("action", Observe.Value.string "user_login");
      ("user_id", Observe.Value.int 42);
      ("login_method", Observe.Value.string "oauth");
      ("remembered", Observe.Value.bool true);
      ("provider", Observe.Value.string "github");
    ]

let nested_untyped () =
  Observe.Value.object_
    [
      ("action", Observe.Value.string "user_login");
      ( "user",
        Observe.Value.object_
          [
            ("id", Observe.Value.int 42);
            ("plan", Observe.Value.string "pro");
            ( "roles",
              Observe.Value.list
                [ Observe.Value.string "admin"; Observe.Value.string "billing" ]
            );
          ] );
      ( "authentication",
        Observe.Value.object_
          [
            ( "Oauth",
              Observe.Value.object_
                [
                  ("provider", Observe.Value.string "github");
                  ( "scopes",
                    Observe.Value.list
                      [
                        Observe.Value.string "openid";
                        Observe.Value.string "profile";
                      ] );
                ] );
          ] );
      ("access", Observe.Value.string "Granted");
      ("remembered", Observe.Value.bool true);
      ("device_id", Observe.Value.string "device_7f3a");
    ]
