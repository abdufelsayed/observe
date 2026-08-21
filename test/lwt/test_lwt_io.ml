module IO = Observe_lwt.IO

let owner_thread = Thread.id (Thread.self ())

let state =
  Observe_lwt.create
    ~clock:(fun () -> Ok (Observe.Timestamp.of_unix_ns 0L))
    ~monotonic_now:(fun () -> Ok 0L)
    ~next_id:(fun () -> Ok "test-operation")
    ~console_style:(fun () -> Observe.Formatter.Plain)
    ~offer_console:(fun _ -> Observe.IO.Accepted)
    ~can_lookup_context:(fun () -> Thread.id (Thread.self ()) = owner_thread)
    ()

let test_dynamic_binding () =
  let key = IO.create_key () in
  let other = IO.create_key () in
  let seen = ref None in
  let promise =
    IO.with_binding state key "capture" (fun () ->
        Lwt.bind (Lwt.pause ()) (fun () ->
            seen := IO.get state key;
            Alcotest.(check (option string))
              "generative key remains empty" None (IO.get state other);
            Lwt.return_unit))
  in
  Lwt_main.run promise;
  Alcotest.(check (option string))
    "binding reaches callback" (Some "capture") !seen;
  Alcotest.(check (option string)) "binding is restored" None (IO.get state key)

let test_exception_restoration () =
  let key = IO.create_key () in
  (match
     IO.with_binding state key "temporary" (fun () ->
         Alcotest.(check (option string))
           "binding is present" (Some "temporary") (IO.get state key);
         raise Exit)
   with
  | exception Exit -> ()
  | _ -> Alcotest.fail "callback exception was not preserved");
  Alcotest.(check (option string))
    "binding restored after exception" None (IO.get state key)

let test_failed_promise_cleanup () =
  let finalized = ref 0 in
  let promise =
    IO.protect state
      ~finally:(fun () -> incr finalized)
      (fun () -> Lwt.fail Exit)
  in
  (match Lwt_main.run promise with
  | exception Exit -> ()
  | _ -> Alcotest.fail "promise rejection was not preserved");
  Alcotest.(check int) "cleanup runs once" 1 !finalized

let test_cancellation_cleanup () =
  let finalized = ref 0 in
  let sleeping = Lwt_unix.sleep 60.0 in
  let promise =
    IO.protect state ~finally:(fun () -> incr finalized) (fun () -> sleeping)
  in
  Lwt.cancel promise;
  (match Lwt.state promise with
  | Lwt.Fail Lwt.Canceled -> ()
  | Lwt.Return _ | Lwt.Sleep | Lwt.Fail _ ->
      Alcotest.fail "cancellation was not preserved");
  Alcotest.(check int) "cleanup runs once" 1 !finalized

let test_control_exception () =
  Alcotest.(check bool)
    "Lwt cancellation is control flow" true
    (IO.is_control_exception state Lwt.Canceled);
  Alcotest.(check bool)
    "ordinary exception is not control flow" false
    (IO.is_control_exception state Exit)

let test_foreign_thread_lookup () =
  let key = IO.create_key () in
  let seen = ref (Some "not-run") in
  IO.with_binding state key "scheduler" (fun () ->
      let thread = Thread.create (fun () -> seen := IO.get state key) () in
      Thread.join thread;
      Alcotest.(check (option string))
        "scheduler binding is unchanged" (Some "scheduler") (IO.get state key);
      Lwt.return_unit)
  |> Lwt_main.run;
  Alcotest.(check (option string))
    "foreign thread has no Lwt context" None !seen

let () =
  Alcotest.run "observe-lwt"
    [
      ( "io",
        [
          Alcotest.test_case "dynamic binding" `Quick test_dynamic_binding;
          Alcotest.test_case "exception restoration" `Quick
            test_exception_restoration;
          Alcotest.test_case "failed promise cleanup" `Quick
            test_failed_promise_cleanup;
          Alcotest.test_case "cancellation cleanup" `Quick
            test_cancellation_cleanup;
          Alcotest.test_case "control exception" `Quick test_control_exception;
          Alcotest.test_case "foreign thread lookup" `Quick
            test_foreign_thread_lookup;
        ] );
    ]
