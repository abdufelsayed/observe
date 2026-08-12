module Observer = Observe.Make (Test_io.IO)

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

let config ?console ?drains () =
  Test_io.config ?console ?drains ~min_level:Observe.Level.Debug "concurrency"

let init_race participants =
  let participants = max 2 participants in
  let observer = Observer.create (Test_io.Host.create ()) in
  let await_start = barrier participants in
  let results = Array.make participants None in
  let threads =
    Array.init participants (fun index ->
        Thread.create
          (fun () ->
            await_start ();
            results.(index) <- Some (Observer.init observer (config ())))
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
        | Some (Error Observe.Already_initialized) -> count + 1 | _ -> count)
      0 results
  in
  Alcotest.(check int) "one publication wins" 1 installed;
  Alcotest.(check int)
    "every other publication loses" (participants - 1) already

let capture_conservation work capacity =
  let work = max 1 work in
  let capacity = max 1 capacity in
  let observer = Observer.create (Test_io.Host.create ()) in
  let result =
    Observer.with_capture observer (config ()) ~capacity (fun capture ->
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
        Test_io.Direct.return capture)
  in
  let capture =
    match result with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "concurrent capture was rejected"
  in
  let retained = List.length (Observe.Capture.logs capture) in
  let overflow =
    Test_io.diagnostic_count
      (Observe.Capture.diagnostics capture)
      Observe.Diagnostics.Capture_overflow
  in
  Alcotest.(check int)
    "capacity retains the available prefix" (min capacity work) retained;
  Alcotest.(check int) "every offer accounted for" work (retained + overflow)

let diagnostic_counting work =
  let work = max 1 work in
  let console_calls = Atomic.make 0 in
  let host =
    Test_io.Host.create
      ~write_console:(fun _ ->
        ignore (Atomic.fetch_and_add console_calls 1 : int);
        Observe.IO.Rejected)
      ()
  in
  let observer = Observer.create host in
  (match Observer.init observer (config ()) with
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
    "console called for every log" work (Atomic.get console_calls);
  Alcotest.(check int)
    "every rejection counted" work
    (Test_io.process_diagnostic_count Observe.Diagnostics.Console_rejected)

let () =
  let mode = if Array.length Sys.argv > 1 then Sys.argv.(1) else "missing" in
  let argument index ~default =
    if Array.length Sys.argv > index then
      Option.value (int_of_string_opt Sys.argv.(index)) ~default
    else default
  in
  let work = argument 2 ~default:1 in
  match mode with
  | "init-race" -> init_race work
  | "capture-conservation" ->
      capture_conservation work (argument 3 ~default:(max 1 (work / 2)))
  | "diagnostic-counting" -> diagnostic_counting work
  | _ -> Alcotest.failf "unknown concurrency scenario: %s" mode
