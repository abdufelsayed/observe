type suite = Component | Core | Lwt_unix | Fs_lwt_unix
type prepared = { operation : unit -> unit; cleanup : unit -> unit }

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
let prepared operation = { operation; cleanup = no_cleanup }

let config ?(environment = "production") ?(console = Observe.Config.Auto)
    ?(min_level = Observe.Level.Debug) ?(drains = []) () =
  Observe.Config.create_exn ~service:"benchmark" ~environment ~console
    ~min_level ~drains ()

let accepted_drain () =
  Observe.Drain.create (fun log ->
      consume log;
      Observe.Drain.Accepted)

let core_operation ?(style = Observe.Formatter.Plain) config make_message =
  let state = Benchmark_io.create ~style () in
  let observer = Observer.create state in
  Observer.init_exn observer config;
  prepared (fun () -> Observe.Logs.info (make_message ()))

let captured_log make_message =
  let observer = Observer.create (Benchmark_io.create ()) in
  match
    Observer.with_capture observer (config ~console:Observe.Config.Silent ())
      ~capacity:1 (fun capture ->
        Observe.Logs.info (make_message ());
        match Observe.Capture.logs capture with
        | [ log ] -> log
        | _ -> failwith "benchmark capture did not retain exactly one log")
  with
  | Ok log -> log
  | Error _ -> failwith "benchmark capture could not register its I/O"

let formatter_operation formatter make_message =
  let log = captured_log make_message in
  prepared (fun () ->
      match Observe.Formatter.format formatter log with
      | Ok output -> consume output
      | Error _ -> failwith "benchmark formatter rejected its fixture")

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

let lwt_unix_operation config make_message =
  let restore = redirect_standard_error () in
  try
    Observe_lwt_unix.init_exn config;
    {
      operation =
        (fun () ->
          Observe.Logs.info (make_message ());
          Lwt_main.run (Observe_lwt_unix.flush ()));
      cleanup =
        (fun () ->
          Fun.protect ~finally:restore (fun () ->
              Lwt_main.run (Observe_lwt_unix.shutdown ())));
    }
  with exception_raised ->
    let backtrace = Printexc.get_raw_backtrace () in
    restore ();
    Printexc.raise_with_backtrace exception_raised backtrace

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

let fs_lwt_unix_operation ~batch_size make_message =
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
            Observe.Logs.info (make_message ())
          done;
          Lwt_main.run (Observe_lwt_unix.flush ()));
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

let make ?(logical_operations = 1) ~name ~suite ~boundary ~payload prepare =
  { name; suite; boundary; payload; logical_operations; prepare }

let text () (m : Observe.Logs.builder) = m.text ~tag:"auth" "user logged in"
let filtered_text () (m : Observe.Logs.builder) = m.text ~tag:"auth" "ignored"

let untyped_small () (m : Observe.Logs.builder) =
  m.untyped (Payload.small_untyped ())

let typed_small () (m : Observe.Logs.builder) =
  m.typed Payload.small_t Payload.small

let untyped_nested () (m : Observe.Logs.builder) =
  m.untyped (Payload.nested_untyped ())

let typed_nested () (m : Observe.Logs.builder) =
  m.typed Payload.nested_t Payload.nested

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
  ]

let fs_lwt_unix_scenarios =
  [
    make ~name:"fs-lwt-unix/completed/tagged-text" ~suite:Fs_lwt_unix
      ~boundary:"completed-write" ~payload:"tagged-text" (fun () ->
        fs_lwt_unix_operation ~batch_size:1 text);
    make ~name:"fs-lwt-unix/completed/typed-small" ~suite:Fs_lwt_unix
      ~boundary:"completed-write" ~payload:"typed-small" (fun () ->
        fs_lwt_unix_operation ~batch_size:1 typed_small);
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
  Fun.protect ~finally:prepared.cleanup (fun () -> callback prepared.operation)
