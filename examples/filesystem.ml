let directory =
  match Array.to_list Sys.argv with
  | [ _; directory ] -> directory
  | _ -> "_build/observe-example-logs"

let main () =
  let open Lwt.Syntax in
  let* filesystem = Observe_fs_lwt_unix.create_exn ~dir:directory () in
  let config =
    Observe.Config.create_exn ~service:"filesystem-example"
      ~environment:"development" ~drains:[ filesystem ] ()
  in
  Observe_lwt_unix.init_exn config;
  [%observe.info text ~tag:"startup" "filesystem delivery is ready"];
  [%observe.info
    untyped { action = "order_created"; order_id = "ord_01JQ9"; items = 3 }];
  let order = Observe.Logs.create ~name:"fulfill-order" () in
  Observe.Logs.info ~operation:order (fun m ->
      m.text ~tag:"fulfillment" "reserving inventory");
  [%observe.set
    order
      {
        order_id = "ord_01JQ9";
        inventory = { status = "reserved"; warehouse = "cairo-1" };
      }];
  Observe.Logs.emit order;
  let* () = Observe_lwt_unix.shutdown () in
  Format.printf "daily NDJSON appended under %s@." directory;
  Lwt.return_unit

let () = Lwt_main.run (main ())
