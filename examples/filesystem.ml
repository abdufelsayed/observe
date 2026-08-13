let path =
  match Array.to_list Sys.argv with
  | [ _; path ] -> path
  | _ -> "_build/observe-example-logs"

let main () =
  let open Lwt.Syntax in
  let* filesystem = Observe_fs_lwt_unix.create_exn ~path () in
  let config =
    Observe.Config.create_exn ~service:"filesystem-example"
      ~environment:"development" ~drains:[ filesystem ] ()
  in
  Observe_lwt_unix.init_exn config;
  Observe.Logs.info
    (Observe.Logs.text ~tag:"startup" "filesystem delivery is ready");
  Observe.Logs.info
    (Observe.Logs.free
       (Observe.Value.object_
          [
            ("action", Observe.Value.string "order_created");
            ("order_id", Observe.Value.string "ord_01JQ9");
            ("items", Observe.Value.int 3);
          ]));
  let* () = Observe_lwt_unix.shutdown () in
  Format.printf "daily NDJSON appended under %s@." path;
  Lwt.return_unit

let () = Lwt_main.run (main ())
