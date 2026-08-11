type event = User_login of { user_id : int; method_ : string }
[@@deriving observe]

let config =
  Observe.Config.create_exn ~service:"example" ~min_level:Observe.Level.Debug ()

let () = Observe_lwt_unix.init_exn config

let main () =
  Observe.Logs.debug (Observe.Logs.text ~tag:"router" "matched route /checkout");
  Observe.Logs.info (Observe.Logs.text ~tag:"auth" "user logged in");
  Observe.Logs.warn (Observe.Logs.text ~tag:"cache" "cache miss for user:42");
  Observe.Logs.error (Observe.Logs.text ~tag:"payment" "payment webhook failed");
  Observe.Logs.info
    (Observe.Logs.free [%observe.value { action = "user_login"; user_id = 42 }]);
  Observe.Logs.info
    (Observe.Logs.structured event_t
       (User_login { user_id = 42; method_ = "oauth" }));
  Lwt.return_unit

let () = Lwt_main.run (main ())
