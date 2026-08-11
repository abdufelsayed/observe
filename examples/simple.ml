type event = User_login of { user_id : int; method_ : string }
[@@deriving observe]

let config = Observe.Config.create_exn ~service:"example" ()
let () = Observe_lwt_unix.init_exn config

let main () =
  Observe.Logs.info (Observe.Logs.text ~tag:"startup" "service ready");
  Observe.Logs.info
    (Observe.Logs.free [%observe.value { action = "user_login"; user_id = 42 }]);
  Observe.Logs.info
    (Observe.Logs.structured event_t
       (User_login { user_id = 42; method_ = "oauth" }));
  Lwt.return_unit

let () = Lwt_main.run (main ())
