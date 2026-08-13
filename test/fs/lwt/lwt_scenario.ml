module Writer = Observe_fs_lwt.Make (Observe_fs_test_support.Fs_fixture.IO)
module Observer = Observe.Make (Observe_lwt.IO)

let fail format = Format.kasprintf failwith format

let install drain =
  let io =
    Observe_lwt.create
      ~clock:(fun () -> Ok (Observe.Timestamp.of_unix_ns 0L))
      ~console_style:(fun () -> Observe.Formatter.Plain)
      ~offer_console:(fun _ -> Observe.IO.Rejected)
      ~can_lookup_context:(fun () -> true)
      ()
  in
  let observer = Observer.create io in
  Observer.init_exn observer
    (Observe.Config.create_exn ~service:"fs-lwt" ~console:Observe.Config.Silent
       ~drains:[ drain ] ())

let cancellation () =
  Observe_fs_test_support.Fs_fixture.reset ();
  let writer = Lwt_main.run (Writer.create ~dir:"/logs" ()) |> Result.get_ok in
  install (Writer.drain writer);
  let release = Observe_fs_test_support.Fs_fixture.block_writes () in
  Observe.Logs.info (fun m -> m.text ~tag:"lwt" "survives cancellation");
  Lwt_main.run (Lwt.pause ());
  let waiting = Writer.flush writer in
  Lwt.cancel waiting;
  release ();
  Lwt_main.run (Writer.shutdown writer) |> Result.get_ok;
  let output =
    Observe_fs_test_support.Fs_fixture.contents "/logs/1970-01-01.jsonl"
  in
  if not (String.contains output '\n') then fail "accepted record was lost"

let () =
  match Array.to_list Sys.argv with
  | [ _; "cancellation" ] -> cancellation ()
  | _ -> fail "unknown Lwt filesystem scenario"
