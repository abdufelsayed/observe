module IO = Observe_lwt.IO
module Fail_open_runtime = Observe.Make (Observe_lwt.IO)

let owner_thread = Thread.id (Thread.self ())

let state =
  Observe_lwt.create
    ~clock:(fun () -> Ok (Observe.Timestamp.of_unix_ns 0L))
    ~monotonic_now:(fun () -> Ok 0L)
    ~next_id:(fun () -> Ok "test-operation")
    ~sampling_draw:(fun () -> 0.)
    ~create_stable_sampling_draw:(fun () -> fun () -> 0.)
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

let test_outcome_and_backtrace () =
  Printexc.record_backtrace true;
  let escaped = Failure "outcome" in
  let original =
    try failwith "outcome-origin"
    with Failure _ -> Printexc.get_raw_backtrace ()
  in
  let outcome =
    Lwt_main.run
      (IO.observe (fun () ->
           Lwt.bind (Lwt.pause ()) (fun () ->
               Printexc.raise_with_backtrace escaped original)))
  in
  match outcome with
  | Observe.IO.Returned _ -> Alcotest.fail "failure became a successful outcome"
  | Observe.IO.Raised (raised, backtrace) ->
      Alcotest.(check bool)
        "outcome preserves exception identity" true (raised == escaped);
      let original = Printexc.raw_backtrace_to_string original in
      let captured = Printexc.raw_backtrace_to_string backtrace in
      Alcotest.(check bool)
        "outcome preserves backtrace origin" true
        (String.length captured >= String.length original
        && String.sub captured 0 (String.length original) = original)

let test_observed_cancellation () =
  let pending, _ = Lwt.task () in
  let observed = IO.observe (fun () -> pending) in
  Lwt.cancel observed;
  match Lwt.state observed with
  | Lwt.Return (Observe.IO.Raised (Lwt.Canceled, _)) -> ()
  | Lwt.Return (Observe.IO.Returned _)
  | Lwt.Return (Observe.IO.Raised _)
  | Lwt.Fail _ | Lwt.Sleep ->
      Alcotest.fail "native cancellation was not observable as an outcome"

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

let diagnostic_count kind =
  Observe.Diagnostics.snapshot ()
  |> List.find_map (fun (entry : Observe.Diagnostics.entry) ->
      if entry.kind = kind then Some entry.count else None)
  |> Option.value ~default:0

let test_stable_sampling_factory_failure () =
  let delivered = ref 0 in
  let independent_draws = ref 0 in
  let before = diagnostic_count Observe.Diagnostics.Sampling_source_raised in
  let state =
    Observe_lwt.create
      ~clock:(fun () -> Ok (Observe.Timestamp.of_unix_ns 0L))
      ~monotonic_now:(fun () -> Ok 0L)
      ~next_id:(fun () -> Ok "stable-factory-failure")
      ~sampling_draw:(fun () ->
        incr independent_draws;
        0.99)
      ~create_stable_sampling_draw:(fun () -> raise Exit)
      ~console_style:(fun () -> Observe.Formatter.Plain)
      ~offer_console:(fun _ -> Observe.IO.Accepted)
      ~can_lookup_context:(fun () -> true)
      ()
  in
  let runtime = Fail_open_runtime.create state in
  let drain =
    Observe.Drain.create (fun _ ->
        incr delivered;
        Observe.Drain.Accepted)
  in
  let sampling =
    Observe.Logs.Sampling.create
      ~info:(Observe.Logs.Sampling.Rate.percent_exn 50.)
      ~stability:Observe.Logs.Sampling.Correlation_stable ()
  in
  let config =
    Observe.Config.create_exn ~service:"sampling-factory-failure"
      ~console:Observe.Config.Silent ~drains:[ drain ] ~sampling ()
  in
  Fail_open_runtime.init_exn runtime config;
  let wide = Observe.Logs.create ~name:"factory-failure" () in
  Observe.Logs.emit wide;
  Fail_open_runtime.close runtime;
  Alcotest.(check int) "factory failure retains the wide log" 1 !delivered;
  Alcotest.(check int)
    "factory failure does not fall back to independent sampling" 0
    !independent_draws;
  Alcotest.(check int)
    "factory failure is diagnosed once" (before + 1)
    (diagnostic_count Observe.Diagnostics.Sampling_source_raised)

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
          Alcotest.test_case "outcome and backtrace" `Quick
            test_outcome_and_backtrace;
          Alcotest.test_case "observed cancellation" `Quick
            test_observed_cancellation;
          Alcotest.test_case "foreign thread lookup" `Quick
            test_foreign_thread_lookup;
          Alcotest.test_case "stable sampling factory failure" `Quick
            test_stable_sampling_factory_failure;
        ] );
    ]
