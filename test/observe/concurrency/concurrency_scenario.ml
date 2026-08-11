module System =
  Observe.Runtime.Make (Test_runtime.Runtime) (Test_runtime.Platform)

let barrier participants =
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  let waiting = ref 0 in
  let released = ref false in
  fun () ->
    Mutex.lock mutex;
    incr waiting;
    if !waiting = participants then (
      released := true;
      Condition.broadcast condition)
    else
      while not !released do
        Condition.wait condition mutex
      done;
    Mutex.unlock mutex

let config ?silent ?drains () =
  Test_runtime.config ?silent ?drains ~min_level:Observe.Level.Debug
    "concurrency"

let init_race participants =
  let participants = max 2 participants in
  let system =
    System.create ~runtime_context:()
      ~platform:(Test_runtime.Platform.create ())
  in
  let await_start = barrier participants in
  let results = Array.make participants None in
  let threads =
    Array.init participants (fun index ->
        Thread.create
          (fun () ->
            await_start ();
            results.(index) <- Some (System.init system (config ())))
          ())
  in
  Array.iter Thread.join threads;
  let installed =
    Array.fold_left
      (fun count -> function Some (Ok ()) -> count + 1 | _ -> count)
      0 results
  in
  let already =
    Array.fold_left
      (fun count -> function
        | Some (Error Observe.Runtime.Already_initialized) -> count + 1
        | _ -> count)
      0 results
  in
  Alcotest.(check int) "one publication wins" 1 installed;
  Alcotest.(check int)
    "every other publication loses" (participants - 1) already

let capture_conservation work =
  let work = max 1 work in
  let capacity = max 1 (work / 2) in
  let system =
    System.create ~runtime_context:()
      ~platform:(Test_runtime.Platform.create ())
  in
  let result =
    System.with_capture system (config ()) ~capacity (fun capture ->
        let await_start = barrier work in
        let threads =
          Array.init work (fun index ->
              Thread.create
                (fun () ->
                  await_start ();
                  Observe.Logs.debug
                    (Observe.Logs.text ~tag:"race" (string_of_int index)))
                ())
        in
        Array.iter Thread.join threads;
        Test_runtime.Runtime.return capture)
  in
  let capture =
    match result with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "concurrent capture was rejected"
  in
  let retained = List.length (Observe.Capture.logs capture) in
  let overflow =
    Test_runtime.diagnostic_count
      (Observe.Capture.diagnostics capture)
      Observe.Diagnostics.Capture_overflow
  in
  Alcotest.(check int) "capacity retained exactly" capacity retained;
  Alcotest.(check int) "every offer accounted for" work (retained + overflow)

let diagnostic_counting work =
  let work = max 1 work in
  let terminal_calls = Atomic.make 0 in
  let platform =
    Test_runtime.Platform.create
      ~write_terminal:(fun _ ->
        ignore (Atomic.fetch_and_add terminal_calls 1 : int);
        Observe.Platform.Rejected)
      ()
  in
  let system = System.create ~runtime_context:() ~platform in
  (match System.init system (config ()) with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "production initialization was rejected");
  let await_start = barrier work in
  let threads =
    Array.init work (fun index ->
        Thread.create
          (fun () ->
            await_start ();
            Observe.Logs.info
              (Observe.Logs.text ~tag:"diagnostic" (string_of_int index)))
          ())
  in
  Array.iter Thread.join threads;
  Alcotest.(check int)
    "terminal called for every log" work
    (Atomic.get terminal_calls);
  Alcotest.(check int)
    "every rejection counted" work
    (Test_runtime.process_diagnostic_count Observe.Diagnostics.Terminal_rejected)

let () =
  let mode = if Array.length Sys.argv > 1 then Sys.argv.(1) else "missing" in
  let work =
    if Array.length Sys.argv > 2 then
      Option.value (int_of_string_opt Sys.argv.(2)) ~default:1
    else 1
  in
  match mode with
  | "init-race" -> init_race work
  | "capture-conservation" -> capture_conservation work
  | "diagnostic-counting" -> diagnostic_counting work
  | _ -> Alcotest.failf "unknown concurrency scenario: %s" mode
