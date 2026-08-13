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
  Observe.Logs.info (fun m ->
      m.text ~tag:"startup" "filesystem delivery is ready");
  Observe.Logs.info (fun m ->
      m.untyped
        (Observe.Value.object_
           [
             ("action", Observe.Value.string "order_created");
             ("order_id", Observe.Value.string "ord_01JQ9");
             ("items", Observe.Value.int 3);
           ]));
  let* () = Observe_lwt_unix.shutdown () in
  Format.printf "daily NDJSON appended under %s@." directory;
  Lwt.return_unit

let () = Lwt_main.run (main ())
