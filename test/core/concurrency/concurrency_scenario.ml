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

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search offset =
    offset + fragment_length <= text_length
    && (String.equal (String.sub text offset fragment_length) fragment
       || search (offset + 1))
  in
  fragment_length = 0 || search 0

let occurrences text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec count offset total =
    if offset + fragment_length > text_length then total
    else if String.equal (String.sub text offset fragment_length) fragment then
      count (offset + fragment_length) (total + 1)
    else count (offset + 1) total
  in
  if fragment_length = 0 then 0 else count 0 0

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
                    (Test_io.text ~tag:"race" (string_of_int index)))
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
      ~offer_console:(fun _ ->
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
              (Test_io.text ~tag:"diagnostic" (string_of_int index)))
          ())
  in
  Array.iter Thread.join threads;
  Alcotest.(check int)
    "console called for every log" work (Atomic.get console_calls);
  Alcotest.(check int)
    "every rejection counted" work
    (Test_io.process_diagnostic_count Observe.Diagnostics.Console_rejected)

let wide_contribution_and_seal work =
  let work = max 2 work in
  let observer = Observer.create (Test_io.Host.create ()) in
  let result =
    Observer.with_capture observer (config ()) (fun capture ->
        let wide = Observe.Logs.create ~name:"concurrent-wide" () in
        let await_set = barrier work in
        let setters =
          Array.init work (fun index ->
              Thread.create
                (fun () ->
                  await_set ();
                  Observe.Logs.set wide (fun m ->
                      let open Observe.Logs in
                      m.untyped
                      |+ m.field
                           ("field_" ^ string_of_int index)
                           Observe.Type.int index
                      |> m.seal))
                ())
        in
        Array.iter Thread.join setters;
        let await_emit = barrier work in
        let emitters =
          Array.init work (fun _ ->
              Thread.create
                (fun () ->
                  await_emit ();
                  Observe.Logs.emit wide)
                ())
        in
        Array.iter Thread.join emitters;
        Test_io.Direct.return capture)
  in
  let capture =
    match result with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "wide capture was rejected"
  in
  match Observe.Capture.logs capture with
  | [ log ] ->
      let json =
        match Observe.Log.body log with
        | Observe.Log.Text _ -> Alcotest.fail "wide body was text"
        | Observe.Log.Structured { value; _ } ->
            Observe.Value.frozen_to_json_string value
      in
      let separators =
        String.fold_left
          (fun count character -> if character = ':' then count + 1 else count)
          0 json
      in
      Alcotest.(check int)
        "every ordered pre-seal contribution is present" work separators;
      Alcotest.(check int)
        "every losing emitter is diagnosed" (work - 1)
        (Test_io.diagnostic_count
           (Observe.Capture.diagnostics capture)
           Observe.Diagnostics.Post_seal_emit)
  | _ -> Alcotest.fail "concurrent emit published more than once"

let wide_set_level_emit_race work =
  let work = max 2 work in
  let observer = Observer.create (Test_io.Host.create ()) in
  let result =
    Observer.with_capture observer (config ()) (fun capture ->
        let wide = Observe.Logs.create ~name:"full-race" () in
        let await_start = barrier (work * 3) in
        let setters =
          Array.init work (fun index ->
              Thread.create
                (fun () ->
                  await_start ();
                  if index = 0 then
                    Observe.Logs.set wide (fun m ->
                        m.error Observe.Error.exn (Failure "raced"))
                  else
                    Observe.Logs.set wide (fun m ->
                        let open Observe.Logs in
                        m.untyped
                        |+ m.field
                             ("contribution_" ^ string_of_int index)
                             Observe.Type.int index
                        |> m.seal))
                ())
        in
        let levelers =
          Array.init work (fun _ ->
              Thread.create
                (fun () ->
                  await_start ();
                  Observe.Logs.set_level wide Observe.Level.Warn)
                ())
        in
        let emitters =
          Array.init work (fun _ ->
              Thread.create
                (fun () ->
                  await_start ();
                  Observe.Logs.emit wide)
                ())
        in
        Array.iter Thread.join setters;
        Array.iter Thread.join levelers;
        Array.iter Thread.join emitters;
        Test_io.Direct.return capture)
  in
  let capture =
    match result with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "full wide race capture was rejected"
  in
  match Observe.Capture.logs capture with
  | [ log ] ->
      let json =
        match Observe.Log.body log with
        | Observe.Log.Text _ -> Alcotest.fail "full wide race body was text"
        | Observe.Log.Structured { value; _ } ->
            Observe.Value.frozen_to_json_string value
      in
      let error_committed = contains json "\"error\":" in
      let ordinary_committed =
        let count = ref 0 in
        for index = 1 to work - 1 do
          if
            contains json
              ("\"contribution_"
              ^ string_of_int index
              ^ "\":"
              ^ string_of_int index)
          then incr count
        done;
        !count
      in
      let diagnostics = Observe.Capture.diagnostics capture in
      let rejected_sets =
        Test_io.diagnostic_count diagnostics Observe.Diagnostics.Post_seal_set
      in
      let rejected_levels =
        Test_io.diagnostic_count diagnostics
          Observe.Diagnostics.Post_seal_set_level
      in
      Alcotest.(check int)
        "every raced contribution is committed or rejected" work
        (ordinary_committed + (if error_committed then 1 else 0) + rejected_sets);
      Alcotest.(check int)
        "exactly one raced emission wins" (work - 1)
        (Test_io.diagnostic_count diagnostics Observe.Diagnostics.Post_seal_emit);
      if rejected_levels < work then
        Alcotest.(check bool)
          "a committed explicit level wins the raced snapshot" true
          (Observe.Level.equal Observe.Level.Warn (Observe.Log.level log))
      else
        Alcotest.(check bool)
          "without a committed explicit level, error meaning and level agree"
          error_committed
          (Observe.Level.equal Observe.Level.Error (Observe.Log.level log))
  | _ -> Alcotest.fail "full wide race did not publish exactly once"

let wide_authoring_linearization () =
  let observer = Observer.create (Test_io.Host.create ()) in
  let result =
    Observer.with_capture observer (config ()) (fun capture ->
        let wide = Observe.Logs.create ~name:"authoring-linearization" () in
        let mutex = Mutex.create () in
        let condition = Condition.create () in
        let entered = ref false in
        let release = ref false in
        let setter =
          Thread.create
            (fun () ->
              Observe.Logs.set wide (fun m ->
                  Mutex.lock mutex;
                  entered := true;
                  Condition.broadcast condition;
                  while not !release do
                    Condition.wait condition mutex
                  done;
                  Mutex.unlock mutex;
                  let open Observe.Logs in
                  m.untyped
                  |+ m.field "reserved" Observe.Type.bool true
                  |> m.seal))
            ()
        in
        Mutex.lock mutex;
        while not !entered do
          Condition.wait condition mutex
        done;
        Mutex.unlock mutex;
        let emitter = Thread.create (fun () -> Observe.Logs.emit wide) () in
        let rec observe_rejection () =
          let before =
            Test_io.diagnostic_count
              (Observe.Capture.diagnostics capture)
              Observe.Diagnostics.Post_seal_set
          in
          let callbacks = ref 0 in
          Observe.Logs.set wide (fun m ->
              incr callbacks;
              let open Observe.Logs in
              m.untyped |+ m.field "late" Observe.Type.bool true |> m.seal);
          let after =
            Test_io.diagnostic_count
              (Observe.Capture.diagnostics capture)
              Observe.Diagnostics.Post_seal_set
          in
          if after > before then
            Alcotest.(check int)
              "post-seal author callback is not evaluated" 0 !callbacks
          else (
            Thread.yield ();
            observe_rejection ())
        in
        observe_rejection ();
        Mutex.lock mutex;
        release := true;
        Condition.broadcast condition;
        Mutex.unlock mutex;
        Thread.join setter;
        Thread.join emitter;
        Test_io.Direct.return capture)
  in
  let capture =
    match result with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "authoring linearization capture was rejected"
  in
  match Observe.Capture.logs capture with
  | [ log ] ->
      let json =
        match Observe.Log.body log with
        | Observe.Log.Text _ -> Alcotest.fail "wide body was text"
        | Observe.Log.Structured { value; _ } ->
            Observe.Value.frozen_to_json_string value
      in
      Alcotest.(check bool)
        "a pre-seal reserved callback contributes before completion" true
        (contains json "\"reserved\":true")
  | _ -> Alcotest.fail "authoring linearization did not publish exactly once"

let wide_parallel_materialization () =
  let observer = Observer.create (Test_io.Host.create ()) in
  let result =
    Observer.with_capture observer (config ()) (fun capture ->
        let wide = Observe.Logs.create ~name:"parallel-materialization" () in
        let first_entered = Atomic.make false in
        let second_entered = Atomic.make false in
        let release = Atomic.make false in
        let rec wait_for entered attempts =
          if Atomic.get entered then true
          else if attempts = 0 then false
          else (
            Thread.yield ();
            wait_for entered (attempts - 1))
        in
        let setter name entered =
          Thread.create
            (fun () ->
              Observe.Logs.set wide (fun m ->
                  Atomic.set entered true;
                  while not (Atomic.get release) do
                    Domain.cpu_relax ()
                  done;
                  let open Observe.Logs in
                  m.untyped |+ m.field name Observe.Type.bool true |> m.seal))
            ()
        in
        let first = setter "first" first_entered in
        if not (wait_for first_entered 1_000_000) then (
          Atomic.set release true;
          Thread.join first;
          Alcotest.fail "first wide author did not start");
        let second = setter "second" second_entered in
        let parallel = wait_for second_entered 1_000_000 in
        Atomic.set release true;
        Thread.join first;
        Thread.join second;
        Alcotest.(check bool)
          "admitted wide callbacks materialize concurrently" true parallel;
        Observe.Logs.emit wide;
        Test_io.Direct.return capture)
  in
  let capture =
    match result with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "parallel materialization capture was rejected"
  in
  match Observe.Capture.logs capture with
  | [ log ] ->
      let json =
        match Observe.Log.body log with
        | Observe.Log.Text _ -> Alcotest.fail "wide body was text"
        | Observe.Log.Structured { value; _ } ->
            Observe.Value.frozen_to_json_string value
      in
      Alcotest.(check bool)
        "both parallel contributions complete before emission" true
        (contains json "\"first\":true" && contains json "\"second\":true")
  | _ -> Alcotest.fail "parallel materialization did not publish exactly once"

let wide_parallel_failure_linearization () =
  let observer = Observer.create (Test_io.Host.create ()) in
  let result =
    Observer.with_capture observer (config ()) (fun capture ->
        let wide = Observe.Logs.create ~name:"parallel-failure" () in
        let first_entered = Atomic.make false in
        let second_entered = Atomic.make false in
        let release = Atomic.make false in
        let rec wait_for entered attempts =
          if Atomic.get entered then true
          else if attempts = 0 then false
          else (
            Thread.yield ();
            wait_for entered (attempts - 1))
        in
        let setter entered =
          Thread.create
            (fun () ->
              Observe.Logs.set wide (fun m ->
                  Atomic.set entered true;
                  while not (Atomic.get release) do
                    Domain.cpu_relax ()
                  done;
                  let open Observe.Logs in
                  m.untyped
                  |+ m.field "invalid" Observe.Type.float Float.nan
                  |> m.seal))
            ()
        in
        let first = setter first_entered in
        if not (wait_for first_entered 1_000_000) then (
          Atomic.set release true;
          Thread.join first;
          Alcotest.fail "first failing wide author did not start");
        let second = setter second_entered in
        let parallel = wait_for second_entered 1_000_000 in
        Atomic.set release true;
        Thread.join first;
        Thread.join second;
        Alcotest.(check bool)
          "failing wide callbacks materialize concurrently" true parallel;
        Observe.Logs.emit wide;
        Test_io.Direct.return capture)
  in
  let capture =
    match result with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "parallel failure capture was rejected"
  in
  Alcotest.(check int)
    "one lifecycle failure is diagnosed" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Canonical_freeze_failed);
  Alcotest.(check int)
    "failed lifecycle publishes nothing" 0
    (List.length (Observe.Capture.logs capture))

let terminal_race work =
  let work = max 3 work in
  let observer = Observer.create (Test_io.Host.create ()) in
  let result =
    Observer.with_capture observer (config ()) (fun capture ->
        let wide = Observe.Logs.create ~name:"terminal-race" () in
        let terminal =
          Observe.Logs.Terminal.create ~error:Observe.Error.exn wide
        in
        let await_start = barrier work in
        let threads =
          Array.init work (fun index ->
              Thread.create
                (fun () ->
                  await_start ();
                  let set (m : Observe.Logs.untyped_builder) =
                    let open Observe.Logs in
                    m.untyped
                    |+ m.field
                         ("terminal_" ^ string_of_int index)
                         Observe.Type.int index
                    |> m.seal
                  in
                  match index mod 3 with
                  | 0 -> Observe.Logs.Terminal.complete terminal ~set ()
                  | 1 ->
                      Observe.Logs.Terminal.fail terminal ~set
                        (Failure "terminal-race")
                  | _ -> Observe.Logs.Terminal.cancel terminal ~set ())
                ())
        in
        Array.iter Thread.join threads;
        Test_io.Direct.return capture)
  in
  let capture =
    match result with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "terminal race capture was rejected"
  in
  match Observe.Capture.logs capture with
  | [ log ] ->
      Alcotest.(check int)
        "terminal race bypasses repeated emit misuse" 0
        (Test_io.diagnostic_count
           (Observe.Capture.diagnostics capture)
           Observe.Diagnostics.Post_seal_emit);
      let json =
        match Observe.Log.body log with
        | Observe.Log.Text _ -> Alcotest.fail "terminal body was text"
        | Observe.Log.Structured { value; _ } ->
            Observe.Value.frozen_to_json_string value
      in
      Alcotest.(check int)
        "only winning terminal callback authors final facts" 1
        (occurrences json "\"terminal_")
  | _ -> Alcotest.fail "terminal race did not publish exactly once"

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
  | "wide-contribution-and-seal" -> wide_contribution_and_seal work
  | "wide-set-level-emit-race" -> wide_set_level_emit_race work
  | "wide-authoring-linearization" -> wide_authoring_linearization ()
  | "wide-parallel-materialization" -> wide_parallel_materialization ()
  | "wide-parallel-failure-linearization" ->
      wide_parallel_failure_linearization ()
  | "terminal-race" -> terminal_race work
  | _ -> Alcotest.failf "unknown concurrency scenario: %s" mode
