type suite = Component | Core | Lwt_unix | Fs_lwt_unix

type prepared = {
  operation : unit -> unit;
  retained_bytes : unit -> float option;
  encoded_bytes : unit -> float option;
  cleanup : unit -> unit;
}

type t = {
  name : string;
  suite : suite;
  boundary : string;
  payload : string;
  logical_operations : int;
  prepare : unit -> prepared;
}

module Observer = Observe.Make (Benchmark_io.IO)

let suite_name = function
  | Component -> "component"
  | Core -> "core"
  | Lwt_unix -> "lwt-unix"
  | Fs_lwt_unix -> "fs-lwt-unix"

let name scenario = scenario.name
let suite scenario = scenario.suite
let boundary scenario = scenario.boundary
let payload scenario = scenario.payload
let logical_operations scenario = scenario.logical_operations
let consume value = ignore (Sys.opaque_identity value)
let no_cleanup () = ()
let no_size () = None

let prepared ?(retained_bytes = no_size) ?(encoded_bytes = no_size) operation =
  { operation; retained_bytes; encoded_bytes; cleanup = no_cleanup }

let config ?(environment = "production") ?(console = Observe.Config.Auto)
    ?(min_level = Observe.Level.Debug) ?(drains = []) () =
  Observe.Config.create_exn ~service:"benchmark" ~environment ~console
    ~min_level ~drains ()

let accepted_drain () =
  Observe.Drain.create (fun log ->
      consume log;
      Observe.Drain.Accepted)

let retained_log_probe () =
  let retained = ref None in
  let drain =
    Observe.Drain.create (fun log ->
        retained := Some log;
        Observe.Drain.Accepted)
  in
  let retained_bytes () =
    Option.map
      (fun log ->
        float_of_int (Obj.reachable_words (Obj.repr log) * (Sys.word_size / 8)))
      !retained
  in
  (drain, retained_bytes)

let core_operation ?(style = Observe.Formatter.Plain) config make_message =
  let state = Benchmark_io.create ~style () in
  let observer = Observer.create state in
  Observer.init_exn observer config;
  prepared (fun () -> Observe.Logs.info (make_message ()))

let retained_core_operation operation =
  let drain, retained_bytes = retained_log_probe () in
  let state = Benchmark_io.create () in
  let observer = Observer.create state in
  Observer.init_exn observer
    (config ~console:Observe.Config.Silent ~drains:[ drain ] ());
  prepared ~retained_bytes operation

let captured_observation emit =
  let observer = Observer.create (Benchmark_io.create ()) in
  match
    Observer.with_capture observer
      ~config:(config ~console:Observe.Config.Silent ()) ~capacity:1
      (fun capture ->
        emit ();
        match Observe.Capture.logs capture with
        | [ log ] -> log
        | _ -> failwith "benchmark capture did not retain exactly one log")
  with
  | Ok log -> log
  | Error _ -> failwith "benchmark capture could not register its I/O"

let captured_log make_message =
  captured_observation (fun () -> Observe.Logs.info (make_message ()))

let formatter_observation formatter emit =
  let log = captured_observation emit in
  let encoded_bytes () =
    match Observe.Formatter.format formatter log with
    | Ok output -> Some (float_of_int (String.length output))
    | Error _ -> None
  in
  prepared ~encoded_bytes (fun () ->
      match Observe.Formatter.format formatter log with
      | Ok output -> consume output
      | Error _ -> failwith "benchmark formatter rejected its fixture")

let formatter_operation formatter make_message =
  formatter_observation formatter (fun () ->
      Observe.Logs.info (make_message ()))

let capture_operation emit =
  let observer = Observer.create (Benchmark_io.create ()) in
  prepared (fun () ->
      match
        Observer.with_capture observer
          ~config:(config ~console:Observe.Config.Silent ()) ~capacity:1
          (fun capture ->
            emit ();
            consume (Observe.Capture.logs capture))
      with
      | Ok () -> ()
      | Error _ -> failwith "benchmark capture could not register its I/O")

let redirect_standard_error () =
  let saved = Unix.dup Unix.stderr in
  let sink = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 sink Unix.stderr;
  Unix.close sink;
  let restored = ref false in
  fun () ->
    if not !restored then (
      restored := true;
      Unix.dup2 saved Unix.stderr;
      Unix.close saved)

let prepare_lwt_unix_operation config emit =
  let restore = redirect_standard_error () in
  try
    Observe_lwt_unix.init_exn config;
    {
      operation =
        (fun () ->
          emit ();
          Lwt_main.run (Observe_lwt_unix.flush ()));
      retained_bytes = no_size;
      encoded_bytes = no_size;
      cleanup =
        (fun () ->
          Fun.protect ~finally:restore (fun () ->
              Lwt_main.run (Observe_lwt_unix.shutdown ())));
    }
  with exception_raised ->
    let backtrace = Printexc.get_raw_backtrace () in
    restore ();
    Printexc.raise_with_backtrace exception_raised backtrace

let lwt_unix_operation config make_message =
  prepare_lwt_unix_operation config (fun () ->
      Observe.Logs.info (make_message ()))

let temporary_directory () =
  let path = Filename.temp_file "observe-bench-fs" ".dir" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  path

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun child -> remove_tree (Filename.concat path child));
      Unix.rmdir path
  | Unix.S_REG | Unix.S_LNK | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO
  | Unix.S_SOCK ->
      Sys.remove path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let prepare_fs_lwt_unix_operation ~batch_size emit =
  let directory = temporary_directory () in
  try
    let drain =
      Lwt_main.run (Observe_fs_lwt_unix.create_exn ~dir:directory ())
    in
    Observe_lwt_unix.init_exn
      (config ~console:Observe.Config.Silent ~drains:[ drain ] ());
    {
      operation =
        (fun () ->
          for _ = 1 to batch_size do
            emit ()
          done;
          Lwt_main.run (Observe_lwt_unix.flush ()));
      retained_bytes = no_size;
      encoded_bytes = no_size;
      cleanup =
        (fun () ->
          Fun.protect
            ~finally:(fun () -> remove_tree directory)
            (fun () -> Lwt_main.run (Observe_lwt_unix.shutdown ())));
    }
  with exception_raised ->
    let backtrace = Printexc.get_raw_backtrace () in
    remove_tree directory;
    Printexc.raise_with_backtrace exception_raised backtrace

let fs_lwt_unix_operation ~batch_size make_message =
  prepare_fs_lwt_unix_operation ~batch_size (fun () ->
      Observe.Logs.info (make_message ()))

let make ?(logical_operations = 1) ~name ~suite ~boundary ~payload prepare =
  { name; suite; boundary; payload; logical_operations; prepare }

let text () (m : Observe.Logs.builder) = m.text ~tag:"auth" "user logged in"
let filtered_text () (m : Observe.Logs.builder) = m.text ~tag:"auth" "ignored"

let untyped_small () (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped
  |+ m.field "action" Observe.Type.string Payload.small.action
  |+ m.field "user_id" Observe.Type.int Payload.small.user_id
  |+ m.field "login_method" Observe.Type.string Payload.small.login_method
  |+ m.field "remembered" Observe.Type.bool Payload.small.remembered
  |+ m.field "provider" Observe.Type.string Payload.small.provider
  |> m.seal

let typed_small () (m : Observe.Logs.builder) =
  m.typed ~using:Payload.small_schema Payload.small

let untyped_nested () (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped
  |+ m.field "action" Observe.Type.string Payload.nested.action
  |+ m.object_ "user" (fun n ->
      n.untyped
      |+ n.field "id" Observe.Type.int Payload.nested.user.id
      |+ n.field "plan" Observe.Type.(option string) Payload.nested.user.plan
      |+ n.field "roles" Observe.Type.(list string) Payload.nested.user.roles
      |> n.seal)
  |+ m.field "authentication" Payload.authentication_t
       Payload.nested.authentication
  |+ m.field "access" Payload.access_t Payload.nested.access
  |+ m.field "remembered" Observe.Type.bool Payload.nested.remembered
  |+ m.field "device_id" Observe.Type.(option string) Payload.nested.device_id
  |> m.seal

let typed_nested () (m : Observe.Logs.builder) =
  m.typed ~using:Payload.nested_schema Payload.nested

let open_small () (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped
  |+ m.field "action" Observe.Type.string "user_login"
  |+ m.field "user_id" Observe.Type.int 42
  |+ m.field "remembered" Observe.Type.bool true
  |> m.seal

let retained_wide_operation operation = retained_core_operation operation

let count_occurrences text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec count offset total =
    if offset + fragment_length > text_length then total
    else if String.equal (String.sub text offset fragment_length) fragment then
      count (offset + fragment_length) (total + 1)
    else count (offset + 1) total
  in
  if fragment_length = 0 then 0 else count 0 0

let validate_contended_log mode expected_fields = function
  | None -> failwith "contended wide benchmark emitted no observation"
  | Some log ->
      let body =
        match Observe.Log.event log with
        | Observe.Log.Text _ ->
            failwith "contended wide benchmark emitted a text observation"
        | Observe.Log.Structured { value; _ } ->
            Observe.Value.frozen_to_json_string value
      in
      let actual =
        match mode with
        | `Accumulate -> count_occurrences body "\"field_"
        | `Replace -> count_occurrences body "\"value\":"
      in
      if actual <> expected_fields then
        failwith
          (Format.asprintf
             "contended wide benchmark retained %d fields; expected %d" actual
             expected_fields)

let contended_wide_operation mode =
  let workers = 4 in
  let contributions_per_worker = 16 in
  let retained = ref None in
  let validate_next = ref true in
  let drain =
    Observe.Drain.create (fun log ->
        if !validate_next then retained := Some log else consume log;
        Observe.Drain.Accepted)
  in
  let state = Benchmark_io.create () in
  let observer = Observer.create state in
  Observer.init_exn observer
    (config ~console:Observe.Config.Silent ~drains:[ drain ] ());
  let mutex = Mutex.create () in
  let ready = Condition.create () in
  let completed = Condition.create () in
  let generation = ref 0 in
  let finished = ref 0 in
  let stopping = ref false in
  let current :
      (Observe.Logs.untyped_builder, Observe.Logs.untyped_patch) Observe.Logs.t
      option
      ref =
    ref None
  in
  let field_names =
    Array.init (workers * contributions_per_worker) (fun index ->
        "field_" ^ string_of_int index)
  in
  let threads =
    Array.init workers (fun worker ->
        Thread.create
          (fun () ->
            let observed = ref 0 in
            let rec work () =
              Mutex.lock mutex;
              while (not !stopping) && !generation = !observed do
                Condition.wait ready mutex
              done;
              if !stopping then Mutex.unlock mutex
              else
                let wide = Option.get !current in
                let assigned = !generation in
                Mutex.unlock mutex;
                for offset = 0 to contributions_per_worker - 1 do
                  let index = (worker * contributions_per_worker) + offset in
                  Observe.Logs.set wide (fun m ->
                      let open Observe.Logs in
                      m.untyped
                      |+ m.field
                           (match mode with
                           | `Accumulate -> field_names.(index)
                           | `Replace -> "value")
                           Observe.Type.int index
                      |> m.seal)
                done;
                Mutex.lock mutex;
                observed := assigned;
                incr finished;
                if !finished = workers then Condition.signal completed;
                Mutex.unlock mutex;
                work ()
            in
            work ())
          ())
  in
  let operation () =
    let wide = Observe.Logs.create ~name:"contended-wide" () in
    Mutex.lock mutex;
    current := Some wide;
    finished := 0;
    incr generation;
    Condition.broadcast ready;
    while !finished < workers do
      Condition.wait completed mutex
    done;
    current := None;
    Mutex.unlock mutex;
    Observe.Logs.emit wide
  in
  operation ();
  validate_contended_log mode
    (match mode with
    | `Accumulate -> workers * contributions_per_worker
    | `Replace -> 1)
    !retained;
  validate_next := false;
  {
    operation;
    retained_bytes = no_size;
    encoded_bytes = no_size;
    cleanup =
      (fun () ->
        Mutex.lock mutex;
        stopping := true;
        Condition.broadcast ready;
        Mutex.unlock mutex;
        Array.iter Thread.join threads);
  }

let retained_core_with_observer make_operation =
  let drain, retained_bytes = retained_log_probe () in
  let state = Benchmark_io.create () in
  let observer = Observer.create state in
  Observer.init_exn observer
    (config ~console:Observe.Config.Silent ~drains:[ drain ] ());
  prepared ~retained_bytes (make_operation observer)

let open_wide_create () = Observe.Logs.create ~name:"open-wide" () |> consume

let open_wide_create_set () =
  let wide = Observe.Logs.create ~name:"open-wide" () in
  Observe.Logs.set wide (fun m ->
      let open Observe.Logs in
      m.untyped
      |+ m.field "action" Observe.Type.string "user_login"
      |+ m.field "user_id" Observe.Type.int 42
      |> m.seal);
  consume wide

let open_wide_create_emit () =
  let wide = Observe.Logs.create ~name:"open-wide" () in
  Observe.Logs.emit wide

let open_wide_repeated () =
  let wide = Observe.Logs.create ~name:"open-wide" () in
  for value = 1 to 4 do
    Observe.Logs.set wide (fun m ->
        let open Observe.Logs in
        m.untyped |+ m.field "value" Observe.Type.int value |> m.seal)
  done;
  Observe.Logs.emit wide

let typed_wide_create () =
  Observe.Logs.create_typed ~name:"typed-wide" ~using:Payload.small_schema ()
  |> consume

let typed_wide_create_set () =
  let wide =
    Observe.Logs.create_typed ~name:"typed-wide" ~using:Payload.small_schema ()
  in
  Observe.Logs.set wide (fun m ->
      m.typed
        (Payload.small_patch ~action:"user_login" ~user_id:42 ~remembered:true
           ()));
  consume wide

let typed_wide_repeated () =
  let wide =
    Observe.Logs.create_typed ~name:"typed-wide" ~using:Payload.small_schema ()
  in
  for user_id = 1 to 4 do
    Observe.Logs.set wide (fun m -> m.typed (Payload.small_patch ~user_id ()))
  done;
  Observe.Logs.emit wide

let field_names = Array.init 64 (fun index -> "field-" ^ string_of_int index)

let open_wide_accumulate count () =
  let wide = Observe.Logs.create ~name:"open-wide-accumulate" () in
  for index = 0 to count - 1 do
    Observe.Logs.set wide (fun m ->
        let open Observe.Logs in
        m.untyped
        |+ m.field field_names.(index) Observe.Type.int index
        |> m.seal)
  done;
  Observe.Logs.emit wide

let open_wide_replace count () =
  let wide = Observe.Logs.create ~name:"open-wide-replace" () in
  for value = 1 to count do
    Observe.Logs.set wide (fun m ->
        let open Observe.Logs in
        m.untyped |+ m.field "value" Observe.Type.int value |> m.seal)
  done;
  Observe.Logs.emit wide

let typed_wide_replace count () =
  let wide =
    Observe.Logs.create_typed ~name:"typed-wide-replace"
      ~using:Payload.small_schema ()
  in
  for user_id = 1 to count do
    Observe.Logs.set wide (fun m -> m.typed (Payload.small_patch ~user_id ()))
  done;
  Observe.Logs.emit wide

let open_wide_error () =
  let wide = Observe.Logs.create ~name:"open-wide" () in
  Observe.Logs.set wide (fun m ->
      m.error ~using:Observe.Error.exn (Failure "benchmark failure"));
  Observe.Logs.emit wide

let open_wide_set_level () =
  let wide = Observe.Logs.create ~name:"open-wide" () in
  Observe.Logs.set_level wide ~level:Observe.Level.Warn;
  Observe.Logs.emit wide

let open_wide_annotate () =
  let wide = Observe.Logs.create ~name:"open-wide" () in
  Observe.Logs.annotate wide ~level:Observe.Level.Warn (fun () ->
      "payment provider is retrying");
  Observe.Logs.emit wide

let explicit_correlated_point wide () =
  Observe.Logs.info ~operation:wide (text ())

let operation_point observer () =
  Observer.with_operation observer ~name:"point-operation" (fun () ->
      Observe.Logs.info (text ()))

let current_open_operation observer () =
  Observer.with_operation observer ~name:"current-open" (fun () ->
      consume (Observe.Logs.current ()))

let current_typed_operation observer () =
  Observer.with_operation observer ~name:"current-typed"
    ~using:Payload.small_schema (fun () ->
      consume (Observe.Logs.current_typed ~using:Payload.small_schema))

let operation_success observer () =
  consume
    (Observer.with_operation observer ~name:"operation-success" (fun () -> 42))

let operation_failure observer () =
  match
    Observer.with_operation observer ~name:"operation-failure" (fun () ->
        raise (Failure "operation benchmark"))
  with
  | _ -> ()
  | exception Failure _ -> ()

let parent_child_operation observer () =
  consume
    (Observer.with_operation observer ~name:"operation-parent" (fun () ->
         Observer.fork observer ~name:"operation-child" (fun () -> 42)))

let lwt_operation_prepare mode =
  let restore = redirect_standard_error () in
  let drain = accepted_drain () in
  try
    Observe_lwt_unix.init_exn
      (config ~console:Observe.Config.Silent ~drains:[ drain ] ());
    let operation () =
      match mode with
      | `Success ->
          consume
            (Lwt_main.run
               (Observe_lwt_unix.with_operation ~name:"lwt-operation-success"
                  (fun () -> Lwt.return 42)))
      | `Failure ->
          Lwt_main.run
            (Lwt.catch
               (fun () ->
                 Observe_lwt_unix.with_operation ~name:"lwt-operation-failure"
                   (fun () -> Lwt.fail (Failure "operation benchmark")))
               (fun _ -> Lwt.return_unit))
      | `Cancellation ->
          let pending, _resolver = Lwt.task () in
          let operation =
            Observe_lwt_unix.with_operation ~name:"lwt-operation-cancel"
              (fun () -> pending)
          in
          Lwt.cancel operation;
          Lwt_main.run
            (Lwt.catch (fun () -> operation) (fun _ -> Lwt.return_unit))
      | `Parent_child ->
          consume
            (Lwt_main.run
               (Observe_lwt_unix.with_operation ~name:"lwt-parent" (fun () ->
                    Observe_lwt_unix.fork ~name:"lwt-child" (fun () ->
                        Lwt.return 42))))
    in
    {
      operation;
      retained_bytes = no_size;
      encoded_bytes = no_size;
      cleanup =
        (fun () ->
          Fun.protect ~finally:restore (fun () ->
              Lwt_main.run (Observe_lwt_unix.shutdown ())));
    }
  with exception_raised ->
    let backtrace = Printexc.get_raw_backtrace () in
    restore ();
    Printexc.raise_with_backtrace exception_raised backtrace

let open_wide () =
  let wide = Observe.Logs.create ~name:"open-wide" () in
  Observe.Logs.set wide (fun m ->
      let open Observe.Logs in
      m.untyped
      |+ m.field "action" Observe.Type.string "user_login"
      |+ m.field "user_id" Observe.Type.int 42
      |> m.seal);
  Observe.Logs.emit wide

let child_wide () =
  let parent = Observe.Logs.create ~name:"parent-wide" () in
  let child = Observe.Logs.create ~parent ~name:"child-wide" () in
  Observe.Logs.set child (fun m ->
      let open Observe.Logs in
      m.untyped
      |+ m.field "action" Observe.Type.string "authorize"
      |+ m.field "result" Observe.Type.string "accepted"
      |> m.seal);
  Observe.Logs.emit child

let typed_wide () =
  let wide =
    Observe.Logs.create_typed ~name:"typed-wide" ~using:Payload.small_schema ()
  in
  Observe.Logs.set wide (fun m ->
      m.typed
        (Payload.small_patch ~action:"user_login" ~user_id:42 ~remembered:true
           ()));
  Observe.Logs.emit wide

let nested_typed_wide () =
  let wide =
    Observe.Logs.create_typed ~name:"nested-wide" ~using:Payload.nested_schema
      ()
  in
  Observe.Logs.set wide (fun m ->
      m.typed
        (Payload.nested_patch ~action:"user_login"
           ~user:
             (Payload.user_patch ~id:42 ~plan:(Some "pro")
                ~roles:[ "admin"; "billing" ] ())
           ~remembered:true ()));
  Observe.Logs.emit wide

let component_scenarios =
  [
    make ~name:"component/untyped-build/small" ~suite:Component
      ~boundary:"untyped-build" ~payload:"small" (fun () ->
        prepared (fun () -> consume (Payload.small_untyped ())));
    make ~name:"component/untyped-build/nested" ~suite:Component
      ~boundary:"untyped-build" ~payload:"nested" (fun () ->
        prepared (fun () -> consume (Payload.nested_untyped ())));
    make ~name:"component/type-json/small" ~suite:Component
      ~boundary:"type-json" ~payload:"small" (fun () ->
        prepared (fun () ->
            consume (Observe.Type.to_json_string Payload.small_t Payload.small)));
    make ~name:"component/type-json/nested" ~suite:Component
      ~boundary:"type-json" ~payload:"nested" (fun () ->
        prepared (fun () ->
            consume
              (Observe.Type.to_json_string Payload.nested_t Payload.nested)));
    make ~name:"component/formatter-json/untyped-small" ~suite:Component
      ~boundary:"formatter-json" ~payload:"untyped-small" (fun () ->
        formatter_operation Observe.Formatter.json untyped_small);
    make ~name:"component/formatter-json/typed-small" ~suite:Component
      ~boundary:"formatter-json" ~payload:"typed-small" (fun () ->
        formatter_operation Observe.Formatter.json typed_small);
    make ~name:"component/formatter-ndjson/untyped-small" ~suite:Component
      ~boundary:"formatter-ndjson" ~payload:"untyped-small" (fun () ->
        formatter_operation Observe.Formatter.ndjson untyped_small);
    make ~name:"component/formatter-json/untyped-nested" ~suite:Component
      ~boundary:"formatter-json" ~payload:"untyped-nested" (fun () ->
        formatter_operation Observe.Formatter.json untyped_nested);
    make ~name:"component/formatter-json/typed-nested" ~suite:Component
      ~boundary:"formatter-json" ~payload:"typed-nested" (fun () ->
        formatter_operation Observe.Formatter.json typed_nested);
    make ~name:"component/formatter-pretty/untyped-nested" ~suite:Component
      ~boundary:"formatter-pretty" ~payload:"untyped-nested" (fun () ->
        formatter_operation
          (Observe.Formatter.pretty Observe.Formatter.Truecolor)
          untyped_nested);
    make ~name:"component/formatter-pretty/typed-nested" ~suite:Component
      ~boundary:"formatter-pretty" ~payload:"typed-nested" (fun () ->
        formatter_operation
          (Observe.Formatter.pretty Observe.Formatter.Truecolor)
          typed_nested);
    make ~name:"component/capture/point-open" ~suite:Component
      ~boundary:"capture-point" ~payload:"open-small" (fun () ->
        capture_operation (fun () -> Observe.Logs.info (open_small ())));
    make ~name:"component/capture/wide-open" ~suite:Component
      ~boundary:"capture-wide" ~payload:"open-wide" (fun () ->
        capture_operation open_wide);
    make ~name:"component/formatter-json/wide-root" ~suite:Component
      ~boundary:"formatter-json" ~payload:"wide-root" (fun () ->
        formatter_observation Observe.Formatter.json open_wide);
    make ~name:"component/formatter-ndjson/wide-root" ~suite:Component
      ~boundary:"formatter-ndjson" ~payload:"wide-root" (fun () ->
        formatter_observation Observe.Formatter.ndjson open_wide);
    make ~name:"component/formatter-pretty/wide-root" ~suite:Component
      ~boundary:"formatter-pretty" ~payload:"wide-root" (fun () ->
        formatter_observation
          (Observe.Formatter.pretty Observe.Formatter.Truecolor)
          open_wide);
    make ~name:"component/formatter-json/wide-child" ~suite:Component
      ~boundary:"formatter-json" ~payload:"wide-child" (fun () ->
        formatter_observation Observe.Formatter.json child_wide);
  ]

let core_scenarios =
  [
    make ~name:"core/filtered/tagged-text" ~suite:Core ~boundary:"filtered"
      ~payload:"tagged-text" (fun () ->
        core_operation (config ~min_level:Observe.Level.Warn ()) filtered_text);
    make ~name:"core/routing/one-drain" ~suite:Core ~boundary:"routing"
      ~payload:"tagged-text" (fun () ->
        core_operation
          (config ~console:Observe.Config.Silent
             ~drains:[ accepted_drain () ]
             ())
          text);
    make ~name:"core/routing/four-drains" ~suite:Core ~boundary:"routing"
      ~payload:"tagged-text" (fun () ->
        core_operation
          (config ~console:Observe.Config.Silent
             ~drains:
               [
                 accepted_drain ();
                 accepted_drain ();
                 accepted_drain ();
                 accepted_drain ();
               ]
             ())
          text);
    make ~name:"core/canonical/typed-point" ~suite:Core
      ~boundary:"canonical-freeze" ~payload:"typed-small" (fun () ->
        retained_core_operation (fun () -> Observe.Logs.info (typed_small ())));
    make ~name:"core/canonical/open-point" ~suite:Core ~boundary:"open-fragment"
      ~payload:"open-small" (fun () ->
        retained_core_operation (fun () -> Observe.Logs.info (open_small ())));
    make ~name:"core/wide/open-fragment" ~suite:Core
      ~boundary:"open-wide-fragment" ~payload:"open-small" (fun () ->
        retained_wide_operation open_wide);
    make ~name:"core/wide-stage/open-create" ~suite:Core ~boundary:"wide-create"
      ~payload:"open-empty" (fun () -> retained_wide_operation open_wide_create);
    make ~name:"core/wide-stage/open-create-set" ~suite:Core
      ~boundary:"wide-create-set" ~payload:"open-small" (fun () ->
        retained_wide_operation open_wide_create_set);
    make ~name:"core/wide-stage/open-create-emit" ~suite:Core
      ~boundary:"wide-create-emit" ~payload:"open-empty" (fun () ->
        retained_wide_operation open_wide_create_emit);
    make ~name:"core/wide-stage/open-repeated" ~suite:Core
      ~boundary:"wide-repeated" ~payload:"open-four-updates" (fun () ->
        retained_wide_operation open_wide_repeated);
    make ~name:"core/wide/typed-patch" ~suite:Core ~boundary:"typed-wide-patch"
      ~payload:"typed-small" (fun () -> retained_wide_operation typed_wide);
    make ~name:"core/wide-stage/typed-create" ~suite:Core
      ~boundary:"wide-create" ~payload:"typed-empty" (fun () ->
        retained_wide_operation typed_wide_create);
    make ~name:"core/wide-stage/typed-create-set" ~suite:Core
      ~boundary:"wide-create-set" ~payload:"typed-small" (fun () ->
        retained_wide_operation typed_wide_create_set);
    make ~name:"core/wide-stage/typed-repeated" ~suite:Core
      ~boundary:"wide-repeated" ~payload:"typed-four-updates" (fun () ->
        retained_wide_operation typed_wide_repeated);
    make ~name:"core/wide-scale/open-accumulate-1" ~suite:Core
      ~boundary:"wide-scale" ~payload:"open-1-field" (fun () ->
        retained_wide_operation (open_wide_accumulate 1));
    make ~name:"core/wide-scale/open-accumulate-4" ~suite:Core
      ~boundary:"wide-scale" ~payload:"open-4-fields" (fun () ->
        retained_wide_operation (open_wide_accumulate 4));
    make ~name:"core/wide-scale/open-accumulate-16" ~suite:Core
      ~boundary:"wide-scale" ~payload:"open-16-fields" (fun () ->
        retained_wide_operation (open_wide_accumulate 16));
    make ~name:"core/wide-scale/open-accumulate-64" ~suite:Core
      ~boundary:"wide-scale" ~payload:"open-64-fields" (fun () ->
        retained_wide_operation (open_wide_accumulate 64));
    make ~name:"core/wide-scale/open-replace-1" ~suite:Core
      ~boundary:"wide-scale" ~payload:"open-1-replacement" (fun () ->
        retained_wide_operation (open_wide_replace 1));
    make ~name:"core/wide-scale/open-replace-4" ~suite:Core
      ~boundary:"wide-scale" ~payload:"open-4-replacements" (fun () ->
        retained_wide_operation (open_wide_replace 4));
    make ~name:"core/wide-scale/open-replace-16" ~suite:Core
      ~boundary:"wide-scale" ~payload:"open-16-replacements" (fun () ->
        retained_wide_operation (open_wide_replace 16));
    make ~name:"core/wide-scale/open-replace-64" ~suite:Core
      ~boundary:"wide-scale" ~payload:"open-64-replacements" (fun () ->
        retained_wide_operation (open_wide_replace 64));
    make ~name:"core/wide-scale/typed-replace-1" ~suite:Core
      ~boundary:"wide-scale" ~payload:"typed-1-replacement" (fun () ->
        retained_wide_operation (typed_wide_replace 1));
    make ~name:"core/wide-scale/typed-replace-4" ~suite:Core
      ~boundary:"wide-scale" ~payload:"typed-4-replacements" (fun () ->
        retained_wide_operation (typed_wide_replace 4));
    make ~name:"core/wide-scale/typed-replace-16" ~suite:Core
      ~boundary:"wide-scale" ~payload:"typed-16-replacements" (fun () ->
        retained_wide_operation (typed_wide_replace 16));
    make ~name:"core/wide-scale/typed-replace-64" ~suite:Core
      ~boundary:"wide-scale" ~payload:"typed-64-replacements" (fun () ->
        retained_wide_operation (typed_wide_replace 64));
    make ~logical_operations:64
      ~name:"core/wide-contention/open-accumulate-4x16" ~suite:Core
      ~boundary:"wide-contention" ~payload:"open-64-fields" (fun () ->
        contended_wide_operation `Accumulate);
    make ~logical_operations:64 ~name:"core/wide-contention/open-replace-4x16"
      ~suite:Core ~boundary:"wide-contention" ~payload:"open-64-replacements"
      (fun () -> contended_wide_operation `Replace);
    make ~name:"core/wide-stage/open-error" ~suite:Core ~boundary:"wide-error"
      ~payload:"open-error" (fun () -> retained_wide_operation open_wide_error);
    make ~name:"core/wide-stage/open-set-level" ~suite:Core
      ~boundary:"wide-level" ~payload:"open-empty" (fun () ->
        retained_wide_operation open_wide_set_level);
    make ~name:"core/wide-stage/open-annotate" ~suite:Core
      ~boundary:"wide-annotation" ~payload:"one-warning" (fun () ->
        retained_wide_operation open_wide_annotate);
    make ~name:"core/correlation/explicit-point" ~suite:Core
      ~boundary:"correlated-point" ~payload:"tagged-text" (fun () ->
        let wide = Observe.Logs.create ~name:"point-parent" () in
        retained_core_operation (explicit_correlated_point wide));
    make ~name:"core/operation/point" ~suite:Core ~boundary:"operation-point"
      ~payload:"tagged-text" (fun () ->
        retained_core_with_observer (fun observer -> operation_point observer));
    make ~name:"core/operation/current-open" ~suite:Core
      ~boundary:"operation-current" ~payload:"open-empty" (fun () ->
        retained_core_with_observer (fun observer ->
            current_open_operation observer));
    make ~name:"core/operation/current-typed" ~suite:Core
      ~boundary:"operation-current" ~payload:"typed-empty" (fun () ->
        retained_core_with_observer (fun observer ->
            current_typed_operation observer));
    make ~name:"core/operation/success" ~suite:Core
      ~boundary:"operation-success" ~payload:"open-empty" (fun () ->
        retained_core_with_observer (fun observer -> operation_success observer));
    make ~name:"core/operation/failure" ~suite:Core
      ~boundary:"operation-failure" ~payload:"open-error" (fun () ->
        retained_core_with_observer (fun observer -> operation_failure observer));
    make ~name:"core/operation/parent-child" ~suite:Core
      ~boundary:"operation-parent-child" ~payload:"open-empty" (fun () ->
        retained_core_with_observer (fun observer ->
            parent_child_operation observer));
    make ~name:"core/wide/nested-patch" ~suite:Core
      ~boundary:"nested-wide-patch" ~payload:"typed-nested" (fun () ->
        retained_wide_operation nested_typed_wide);
    make ~name:"core/json/tagged-text" ~suite:Core ~boundary:"json"
      ~payload:"tagged-text" (fun () ->
        core_operation (config ~console:Observe.Config.Ndjson ()) text);
    make ~name:"core/json/untyped-small" ~suite:Core ~boundary:"json"
      ~payload:"untyped-small" (fun () ->
        core_operation (config ~console:Observe.Config.Ndjson ()) untyped_small);
    make ~name:"core/json/typed-small" ~suite:Core ~boundary:"json"
      ~payload:"typed-small" (fun () ->
        core_operation (config ~console:Observe.Config.Ndjson ()) typed_small);
    make ~name:"core/json/untyped-nested" ~suite:Core ~boundary:"json"
      ~payload:"untyped-nested" (fun () ->
        core_operation (config ~console:Observe.Config.Ndjson ()) untyped_nested);
    make ~name:"core/json/typed-nested" ~suite:Core ~boundary:"json"
      ~payload:"typed-nested" (fun () ->
        core_operation (config ~console:Observe.Config.Ndjson ()) typed_nested);
    make ~name:"core/pretty/tagged-text" ~suite:Core ~boundary:"pretty"
      ~payload:"tagged-text" (fun () ->
        core_operation ~style:Observe.Formatter.Truecolor
          (config ~environment:"development" ~console:Observe.Config.Pretty ())
          text);
    make ~name:"core/pretty/untyped-nested" ~suite:Core ~boundary:"pretty"
      ~payload:"untyped-nested" (fun () ->
        core_operation ~style:Observe.Formatter.Truecolor
          (config ~environment:"development" ~console:Observe.Config.Pretty ())
          untyped_nested);
    make ~name:"core/pretty/typed-nested" ~suite:Core ~boundary:"pretty"
      ~payload:"typed-nested" (fun () ->
        core_operation ~style:Observe.Formatter.Truecolor
          (config ~environment:"development" ~console:Observe.Config.Pretty ())
          typed_nested);
  ]

let lwt_unix_scenarios =
  [
    make ~name:"lwt-unix/json/tagged-text" ~suite:Lwt_unix ~boundary:"json"
      ~payload:"tagged-text" (fun () ->
        lwt_unix_operation (config ~console:Observe.Config.Ndjson ()) text);
    make ~name:"lwt-unix/json/untyped-small" ~suite:Lwt_unix ~boundary:"json"
      ~payload:"untyped-small" (fun () ->
        lwt_unix_operation
          (config ~console:Observe.Config.Ndjson ())
          untyped_small);
    make ~name:"lwt-unix/json/typed-small" ~suite:Lwt_unix ~boundary:"json"
      ~payload:"typed-small" (fun () ->
        lwt_unix_operation
          (config ~console:Observe.Config.Ndjson ())
          typed_small);
    make ~name:"lwt-unix/pretty/untyped-nested" ~suite:Lwt_unix
      ~boundary:"pretty" ~payload:"untyped-nested" (fun () ->
        lwt_unix_operation
          (config ~environment:"development" ~console:Observe.Config.Pretty ())
          untyped_nested);
    make ~name:"lwt-unix/pretty/typed-nested" ~suite:Lwt_unix ~boundary:"pretty"
      ~payload:"typed-nested" (fun () ->
        lwt_unix_operation
          (config ~environment:"development" ~console:Observe.Config.Pretty ())
          typed_nested);
    make ~name:"lwt-unix/json/wide-root" ~suite:Lwt_unix ~boundary:"json"
      ~payload:"wide-root" (fun () ->
        prepare_lwt_unix_operation
          (config ~console:Observe.Config.Ndjson ())
          open_wide);
    make ~name:"lwt-unix/pretty/wide-child" ~suite:Lwt_unix ~boundary:"pretty"
      ~payload:"wide-child" (fun () ->
        prepare_lwt_unix_operation
          (config ~environment:"development" ~console:Observe.Config.Pretty ())
          child_wide);
    make ~name:"lwt-unix/operation/success" ~suite:Lwt_unix
      ~boundary:"operation-success" ~payload:"open-empty" (fun () ->
        lwt_operation_prepare `Success);
    make ~name:"lwt-unix/operation/failure" ~suite:Lwt_unix
      ~boundary:"operation-failure" ~payload:"open-error" (fun () ->
        lwt_operation_prepare `Failure);
    make ~name:"lwt-unix/operation/cancellation" ~suite:Lwt_unix
      ~boundary:"operation-cancellation" ~payload:"open-empty" (fun () ->
        lwt_operation_prepare `Cancellation);
    make ~name:"lwt-unix/operation/parent-child" ~suite:Lwt_unix
      ~boundary:"operation-parent-child" ~payload:"open-empty" (fun () ->
        lwt_operation_prepare `Parent_child);
  ]

let fs_lwt_unix_scenarios =
  [
    make ~name:"fs-lwt-unix/completed/tagged-text" ~suite:Fs_lwt_unix
      ~boundary:"completed-write" ~payload:"tagged-text" (fun () ->
        fs_lwt_unix_operation ~batch_size:1 text);
    make ~name:"fs-lwt-unix/completed/typed-small" ~suite:Fs_lwt_unix
      ~boundary:"completed-write" ~payload:"typed-small" (fun () ->
        fs_lwt_unix_operation ~batch_size:1 typed_small);
    make ~name:"fs-lwt-unix/completed/wide-root" ~suite:Fs_lwt_unix
      ~boundary:"completed-write" ~payload:"wide-root" (fun () ->
        prepare_fs_lwt_unix_operation ~batch_size:1 open_wide);
    make ~logical_operations:100 ~name:"fs-lwt-unix/batch-100/tagged-text"
      ~suite:Fs_lwt_unix ~boundary:"amortized-write" ~payload:"tagged-text"
      (fun () -> fs_lwt_unix_operation ~batch_size:100 text);
    make ~logical_operations:100 ~name:"fs-lwt-unix/batch-100/untyped-small"
      ~suite:Fs_lwt_unix ~boundary:"amortized-write" ~payload:"untyped-small"
      (fun () -> fs_lwt_unix_operation ~batch_size:100 untyped_small);
    make ~logical_operations:100 ~name:"fs-lwt-unix/batch-100/typed-small"
      ~suite:Fs_lwt_unix ~boundary:"amortized-write" ~payload:"typed-small"
      (fun () -> fs_lwt_unix_operation ~batch_size:100 typed_small);
    make ~logical_operations:100 ~name:"fs-lwt-unix/batch-100/typed-nested"
      ~suite:Fs_lwt_unix ~boundary:"amortized-write" ~payload:"typed-nested"
      (fun () -> fs_lwt_unix_operation ~batch_size:100 typed_nested);
  ]

let all =
  component_scenarios
  @ core_scenarios
  @ lwt_unix_scenarios
  @ fs_lwt_unix_scenarios

let find wanted = List.find_opt (fun scenario -> scenario.name = wanted) all

let with_operation scenario callback =
  let prepared = scenario.prepare () in
  Fun.protect ~finally:prepared.cleanup (fun () ->
      callback prepared.operation prepared.retained_bytes prepared.encoded_bytes)
