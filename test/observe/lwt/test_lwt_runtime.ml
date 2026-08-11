module Runtime = Observe_lwt.Runtime

let test_dynamic_binding () =
  let key = Runtime.create_key () in
  let other = Runtime.create_key () in
  let seen = ref None in
  let promise =
    Runtime.with_binding () key "capture" (fun () ->
        Lwt.bind (Lwt.pause ()) (fun () ->
            seen := Runtime.get () key;
            Alcotest.(check (option string))
              "generative key remains empty" None (Runtime.get () other);
            Lwt.return_unit))
  in
  Lwt_main.run promise;
  Alcotest.(check (option string))
    "binding reaches callback" (Some "capture") !seen;
  Alcotest.(check (option string))
    "binding is restored" None (Runtime.get () key)

let test_exception_restoration () =
  let key = Runtime.create_key () in
  (match
     Runtime.with_binding () key "temporary" (fun () ->
         Alcotest.(check (option string))
           "binding is present" (Some "temporary") (Runtime.get () key);
         raise Exit)
   with
  | exception Exit -> ()
  | _ -> Alcotest.fail "callback exception was not preserved");
  Alcotest.(check (option string))
    "binding restored after exception" None (Runtime.get () key)

let test_failed_promise_cleanup () =
  let finalized = ref 0 in
  let promise =
    Runtime.protect ()
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
    Runtime.protect () ~finally:(fun () -> incr finalized) (fun () -> sleeping)
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
    (Runtime.is_control_exception () Lwt.Canceled);
  Alcotest.(check bool)
    "ordinary exception is not control flow" false
    (Runtime.is_control_exception () Exit)

let () =
  Alcotest.run "observe-lwt"
    [
      ( "runtime",
        [
          Alcotest.test_case "dynamic binding" `Quick test_dynamic_binding;
          Alcotest.test_case "exception restoration" `Quick
            test_exception_restoration;
          Alcotest.test_case "failed promise cleanup" `Quick
            test_failed_promise_cleanup;
          Alcotest.test_case "cancellation cleanup" `Quick
            test_cancellation_cleanup;
          Alcotest.test_case "control exception" `Quick test_control_exception;
        ] );
    ]
