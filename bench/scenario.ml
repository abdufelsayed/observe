type suite = Component | Core | Fs | Lwt_unix | Fs_lwt_unix

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

module Fs_benchmark_io = struct
  include Observe_fs_test_support.Fs_fixture.IO

  type 'a t = 'a Lwt.t

  let return = Lwt.return
  let bind = Lwt.bind
  let catch = Lwt.catch
  let async = Lwt.async
end

module Fs_benchmark_writer = Observe_fs.Make (Fs_benchmark_io)

let suite_name = function
  | Component -> "component"
  | Core -> "core"
  | Fs -> "fs"
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
    ?(min_level = Observe.Level.Debug) ?(drains = []) ?enrichers ?limits
    ?redaction ?sampling ?retention () =
  Observe.Config.create_exn ~service:"benchmark" ~environment ~console
    ~min_level ~drains ?enrichers ?limits ?redaction ?sampling ?retention ()

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

let core_operation ?(style = Observe.Formatter.Plain)
    ?(sampling_draw = fun () -> 0.) config make_message =
  let state = Benchmark_io.create ~style ~sampling_draw () in
  let observer = Observer.create state in
  Observer.init_exn observer config;
  prepared (fun () -> Observe.Logs.info (make_message ()))

let core_author_operation ?(sampling_draw = fun () -> 0.) config author =
  let state = Benchmark_io.create ~sampling_draw () in
  let observer = Observer.create state in
  Observer.init_exn observer config;
  prepared (fun () -> Observe.Logs.info author)

let core_unit_operation config operation =
  let state = Benchmark_io.create () in
  let observer = Observer.create state in
  Observer.init_exn observer config;
  prepared operation

let withheld_core_operation limits make_message =
  let observed = ref false in
  let drain =
    Observe.Drain.create (fun _ ->
        observed := true;
        Observe.Drain.Accepted)
  in
  let state = Benchmark_io.create () in
  let observer = Observer.create state in
  Observer.init_exn observer
    (config ~console:Observe.Config.Silent ~drains:[ drain ] ~limits ());
  let operation () = Observe.Logs.info (make_message ()) in
  operation ();
  if !observed then
    failwith "full-withholding benchmark unexpectedly retained an observation";
  prepared operation

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

let prepare_lwt_unix_operation ?sampling_draw ?(flush = true) config emit =
  let restore = redirect_standard_error () in
  try
    Observe_lwt_unix.init_exn ?sampling_draw config;
    {
      operation =
        (fun () ->
          emit ();
          if flush then Lwt_main.run (Observe_lwt_unix.flush ()));
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

let prepare_lifecycle_flush ?facts () =
  let restore = redirect_standard_error () in
  try
    Observe_lwt_unix.init_exn
      (config ~console:Observe.Config.Silent ~drains:[ accepted_drain () ] ());
    Option.iter
      (fun facts ->
        Observe_lwt_unix.Lifecycle.Integration.register ~label:"benchmark"
          ~facts
          ~flush:(fun () -> Lwt.return_unit)
          ~shutdown:(fun () -> Lwt.return_unit)
        |> Result.get_ok)
      facts;
    {
      operation =
        (fun () ->
          ignore
            (Lwt_main.run (Observe_lwt_unix.Lifecycle.flush ())
              : Observe_lwt_unix.Lifecycle.report));
      retained_bytes = no_size;
      encoded_bytes = no_size;
      cleanup =
        (fun () ->
          Fun.protect ~finally:restore (fun () ->
              ignore
                (Lwt_main.run (Observe_lwt_unix.Lifecycle.shutdown ())
                  : Observe_lwt_unix.Lifecycle.report)));
    }
  with exception_raised ->
    let backtrace = Printexc.get_raw_backtrace () in
    restore ();
    Printexc.raise_with_backtrace exception_raised backtrace

let prepare_settled_shutdown () =
  let prepared = prepare_lifecycle_flush () in
  ignore
    (Lwt_main.run (Observe_lwt_unix.Lifecycle.shutdown ())
      : Observe_lwt_unix.Lifecycle.report);
  {
    prepared with
    operation =
      (fun () ->
        ignore
          (Lwt_main.run (Observe_lwt_unix.Lifecycle.shutdown ())
            : Observe_lwt_unix.Lifecycle.report));
  }

let lwt_stable_fan_in () =
  let wide = Observe.Logs.create ~name:"stable-fan-in" () in
  for _ = 1 to 8 do
    Observe.Logs.info ~operation:wide (fun builder ->
        builder.text ~tag:"sampling" "correlated point")
  done;
  Observe.Logs.emit wide

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

let prepare_fs_rejection mode make_message =
  Observe_fs_test_support.Fs_fixture.reset ();
  let writer =
    Lwt_main.run (Fs_benchmark_writer.create ~dir:"/logs" ~capacity:1 ())
    |> Result.get_ok
  in
  let drain = Fs_benchmark_writer.drain writer in
  let observer = Observer.create (Benchmark_io.create ()) in
  Observer.init_exn observer
    (config ~console:Observe.Config.Silent ~drains:[ drain ] ());
  let emit () = Observe.Logs.info (make_message ()) in
  let cleanup = ref (fun () -> ()) in
  (match mode with
  | `Full ->
      let release = Observe_fs_test_support.Fs_fixture.block_writes () in
      cleanup := release;
      emit ();
      Lwt_main.run (Lwt.pause ());
      emit ()
  | `Closed ->
      Lwt_main.run (Fs_benchmark_writer.shutdown writer) |> Result.get_ok);
  let rejected_before =
    Observe.Diagnostics.snapshot ()
    |> List.find_map (fun (entry : Observe.Diagnostics.entry) ->
        if entry.kind = Observe.Diagnostics.Drain_rejected then Some entry.count
        else None)
    |> Option.value ~default:0
  in
  emit ();
  let rejected_after =
    Observe.Diagnostics.snapshot ()
    |> List.find_map (fun (entry : Observe.Diagnostics.entry) ->
        if entry.kind = Observe.Diagnostics.Drain_rejected then Some entry.count
        else None)
    |> Option.value ~default:0
  in
  if rejected_after <> rejected_before + 1 then
    failwith "filesystem rejection benchmark did not reject its probe";
  {
    operation = emit;
    retained_bytes = no_size;
    encoded_bytes = no_size;
    cleanup =
      (fun () ->
        !cleanup ();
        Lwt_main.run (Fs_benchmark_writer.shutdown writer) |> Result.get_ok;
        Observer.close observer);
  }

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

let redaction_token () (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped |+ m.field "token" Observe.Type.string "secret-open" |> m.seal

let redaction_tokens count () (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped
  |+ m.field "tokens"
       Observe.Type.(list string)
       (List.init count (fun index -> Printf.sprintf "secret-%04d" index))
  |> m.seal

let redaction_text () (m : Observe.Logs.builder) =
  m.text ~tag:"checkout" "%s" "secret-point"

let redaction_annotation () =
  let wide = Observe.Logs.create ~name:"redaction-annotation" () in
  Observe.Logs.annotate wide ~level:Observe.Level.Info (fun () ->
      "secret-annotation");
  Observe.Logs.emit wide

let redaction_open_wide_nested () =
  let wide = Observe.Logs.create ~name:"redaction-open-wide" () in
  Observe.Logs.set wide (fun m ->
      let open Observe.Logs in
      m.untyped
      |+ m.object_ "user" (fun user ->
          user.untyped
          |+ user.field "plan" Observe.Type.string "pro"
          |> user.seal)
      |> m.seal);
  Observe.Logs.emit wide

let redaction_path fields = Observe.Logs.Redaction.Path.fields fields
let redaction_policy rules = Observe.Logs.Redaction.create_exn ~rules ()

let redaction_matching prefix action =
  Observe.Logs.Redaction.Rule.matching
    (Observe.Logs.Redaction.Matcher.string_prefix prefix)
    action

let redaction_exact fields action =
  Observe.Logs.Redaction.Rule.at (redaction_path fields) action

let redaction_exact_nested_remove =
  redaction_policy
    [ redaction_exact [ "user"; "plan" ] Observe.Logs.Redaction.Action.remove ]

let redaction_exact_typed_nested_remove =
  Observe.Logs.Redaction.create_exn ~using:Payload.nested_schema
    ~rules:
      [
        redaction_exact [ "user"; "plan" ] Observe.Logs.Redaction.Action.remove;
      ]
    ()

let redaction_unmatched =
  redaction_policy
    [
      redaction_matching "never-"
        (Observe.Logs.Redaction.Action.replace
           (Observe.Value.string "[unmatched]"));
    ]

let redaction_unmatched_prefixes count =
  let replacement =
    Observe.Logs.Redaction.Action.replace (Observe.Value.string "[unmatched]")
  in
  redaction_policy
    (List.init count (fun index ->
         redaction_matching
           (Printf.sprintf "never-prefix-%04d-" index)
           replacement))

let redaction_unmatched_contains count =
  let replacement =
    Observe.Logs.Redaction.Action.replace (Observe.Value.string "[unmatched]")
  in
  redaction_policy
    (List.init count (fun index ->
         Observe.Logs.Redaction.Rule.matching
           (Observe.Logs.Redaction.Matcher.string_contains
              (Printf.sprintf "never-contains-%04d" index))
           replacement))

let redaction_unmatched_prefixes_256 = redaction_unmatched_prefixes 256
let redaction_unmatched_contains_256 = redaction_unmatched_contains 256

let redaction_unmatched_exact_256 =
  redaction_policy
    (List.init 256 (fun index ->
         redaction_exact
           [ Printf.sprintf "absent-%04d" index ]
           Observe.Logs.Redaction.Action.remove))

let redaction_matching_replace =
  redaction_policy
    [
      redaction_matching "secret-"
        (Observe.Logs.Redaction.Action.replace
           (Observe.Value.string "[matched]"));
    ]

let redaction_finite_mask =
  let mask =
    Observe.Logs.Redaction.Mask.keep_suffix ~characters:4
      ~hidden:(Observe.Logs.Redaction.Mask.Fill "*") ()
  in
  redaction_policy
    [ redaction_exact [ "token" ] (Observe.Logs.Redaction.Action.mask mask) ]

let redaction_custom_mask =
  let mask =
    Observe.Logs.Redaction.Mask.custom (fun value ->
        String.make (String.length value) '*')
  in
  redaction_policy
    [ redaction_exact [ "token" ] (Observe.Logs.Redaction.Action.mask mask) ]

let redaction_failing_custom_mask =
  let mask =
    Observe.Logs.Redaction.Mask.custom (fun _ -> raise (Failure "benchmark"))
  in
  redaction_policy
    [ redaction_exact [ "token" ] (Observe.Logs.Redaction.Action.mask mask) ]

let redaction_drain_policy =
  redaction_policy
    [ redaction_exact [ "token" ] Observe.Logs.Redaction.Action.remove ]

let redaction_core_operation ?(redaction = Observe.Logs.Redaction.none)
    make_message =
  core_operation
    (config ~console:Observe.Config.Silent
       ~drains:[ accepted_drain () ]
       ~redaction ())
    make_message

let redaction_core_unit_operation ?(redaction = Observe.Logs.Redaction.none)
    operation =
  core_unit_operation
    (config ~console:Observe.Config.Silent
       ~drains:[ accepted_drain () ]
       ~redaction ())
    operation

let redaction_stricter_drain_operation make_message =
  let drain =
    Observe.Drain.create (fun log ->
        consume log;
        Observe.Drain.Accepted)
    |> Observe.Drain.with_redaction ~redaction:redaction_drain_policy
  in
  core_operation
    (config ~console:Observe.Config.Silent ~drains:[ drain ] ())
    make_message

let info_half =
  Observe.Logs.Sampling.create
    ~info:(Observe.Logs.Sampling.Rate.percent_exn 50.)
    ()

let info_never =
  Observe.Logs.Sampling.create ~info:Observe.Logs.Sampling.Rate.never ()

let retain_completed = Observe.Logs.Retention.create ~keep:(fun _ -> true)
let defer_completed = Observe.Logs.Retention.create ~keep:(fun _ -> false)

let inspect_completed =
  Observe.Logs.Retention.create ~keep:(fun log ->
      match
        Observe.Value.find [ "token" ] (Observe.Log.fields log)
        |> Option.map Observe.Value.view
      with
      | Some (`String "secret-open") -> true
      | _ -> false)

let routed_drain predicate =
  accepted_drain () |> Observe.Drain.with_route ~when_:predicate

let routed_stricter_drain predicate =
  accepted_drain ()
  |> Observe.Drain.with_redaction ~redaction:redaction_drain_policy
  |> Observe.Drain.with_route ~when_:predicate

let benchmark_enricher name fields =
  Observe.Logs.Enricher.create_exn ~name (fun () ->
      Observe.Value.object_ fields)

let benchmark_enricher_one =
  benchmark_enricher "benchmark-one"
    [ ("deployment", Observe.Value.string "production") ]

let benchmark_enricher_two =
  benchmark_enricher "benchmark-two"
    [
      ("region", Observe.Value.string "test");
      ("release", Observe.Value.string "candidate");
    ]

let benchmark_enrichers_many =
  List.init 8 (fun index ->
      benchmark_enricher
        ("benchmark-many-" ^ string_of_int index)
        [ ("context_" ^ string_of_int index, Observe.Value.int index) ])

let ppx_embedded_typed () =
  [%observe.info
    untyped
      { payload = [%observe.value.embed Payload.nested_t, Payload.nested] }]

let rec nested_object depth (m : Observe.Logs.untyped_builder) =
  let open Observe.Logs in
  if depth <= 0 then m.seal m.untyped
  else
    m.untyped
    |+ m.object_ "child" (fun child -> nested_object (depth - 1) child)
    |> m.seal

let nested_structured depth () (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped
  |+ m.object_ "root" (fun child -> nested_object depth child)
  |> m.seal

let wide_structured count () (m : Observe.Logs.builder) =
  let open Observe.Logs in
  let rec add_fields object_ index =
    if index = count then m.seal object_
    else
      add_fields
        (object_
        |+ m.field ("field_" ^ string_of_int index) Observe.Type.int index)
        (index + 1)
  in
  add_fields m.untyped 0

let collection_structured count () (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped
  |+ m.field "items"
       (Observe.Type.list Observe.Type.int)
       (List.init count (fun index -> index))
  |> m.seal

let bytes_structured length () (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped
  |+ m.field "payload" Observe.Type.bytes (Bytes.make length 'x')
  |> m.seal

let limited_string () (m : Observe.Logs.builder) =
  let open Observe.Logs in
  m.untyped
  |+ m.field "value" Observe.Type.string (String.make 65 'x')
  |> m.seal

let p16_limits =
  Observe.Logs.Limits.create_exn ~max_string_bytes:64 ~max_total_bytes:100_000
    ()

let p16_depth_limits =
  Observe.Logs.Limits.create_exn ~max_depth:8 ~max_total_bytes:100_000 ()

let p16_width_limits =
  Observe.Logs.Limits.create_exn ~max_object_fields:32 ~max_total_bytes:100_000
    ()

let p16_collection_limits =
  Observe.Logs.Limits.create_exn ~max_collection_length:32
    ~max_total_bytes:100_000 ()

let p16_node_limits =
  Observe.Logs.Limits.create_exn ~max_nodes:32 ~max_total_bytes:100_000 ()

let p16_byte_limits =
  Observe.Logs.Limits.create_exn ~max_bytes_length:64 ~max_total_bytes:100_000
    ()

let p16_withholding_limits =
  Observe.Logs.Limits.create_exn ~max_total_bytes:1 ()

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
        | Observe.Log.Structured _ ->
            Observe.Value.frozen_to_json_string (Observe.Log.fields log)
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

let retained_correlated_point_operation () =
  let drain, retained_bytes = retained_log_probe () in
  let sampling =
    Observe.Logs.Sampling.create
      ~info:(Observe.Logs.Sampling.Rate.percent_exn 50.)
      ~stability:Observe.Logs.Sampling.Correlation_stable ()
  in
  let state = Benchmark_io.create ~sampling_draw:(fun () -> 0.25) () in
  let observer = Observer.create state in
  Observer.init_exn observer
    (config ~console:Observe.Config.Silent ~drains:[ drain ] ~sampling ());
  let wide = Observe.Logs.create ~name:"point-parent" () in
  prepared ~retained_bytes (explicit_correlated_point wide)

let rejected_wide_operation emit =
  let state = Benchmark_io.create () in
  let observer = Observer.create state in
  Observer.init_exn observer
    (config ~console:Observe.Config.Silent
       ~drains:[ accepted_drain () ]
       ~sampling:info_never ());
  prepared emit

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

let fs_scenarios =
  [
    make ~name:"fs/rejected/full" ~suite:Fs ~boundary:"prompt-rejection"
      ~payload:"tagged-text" (fun () -> prepare_fs_rejection `Full text);
    make ~name:"fs/rejected/closed" ~suite:Fs ~boundary:"prompt-rejection"
      ~payload:"tagged-text" (fun () -> prepare_fs_rejection `Closed text);
  ]

let core_scenarios =
  let scenarios =
    [
      make ~name:"core/filtered/tagged-text" ~suite:Core ~boundary:"filtered"
        ~payload:"tagged-text" (fun () ->
          core_operation (config ~min_level:Observe.Level.Warn ()) filtered_text);
      make ~name:"core/retention/baseline" ~suite:Core
        ~boundary:"retention-baseline" ~payload:"tagged-text" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ())
            text);
      make ~name:"core/retention/sample-never" ~suite:Core
        ~boundary:"sampling-early-rejection" ~payload:"tagged-text" (fun () ->
          core_author_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~sampling:info_never ())
            (text ()));
      make ~name:"core/retention/wide-sample-never" ~suite:Core
        ~boundary:"sampling-wide-final-rejection" ~payload:"open-wide"
        (fun () -> rejected_wide_operation open_wide);
      make ~name:"core/retention/sample-drop" ~suite:Core
        ~boundary:"sampling-base-rejection" ~payload:"tagged-text" (fun () ->
          core_operation
            ~sampling_draw:(fun () -> 0.75)
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~sampling:info_half ())
            text);
      make ~name:"core/retention/sample-keep" ~suite:Core
        ~boundary:"sampling-base-retention" ~payload:"tagged-text" (fun () ->
          core_operation
            ~sampling_draw:(fun () -> 0.25)
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~sampling:info_half ())
            text);
      make ~name:"core/retention/completion-rescue" ~suite:Core
        ~boundary:"sampling-completion-rescue" ~payload:"tagged-text" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~sampling:info_never ~retention:retain_completed ())
            text);
      make ~name:"core/retention/completion-defer" ~suite:Core
        ~boundary:"sampling-completion-defer" ~payload:"tagged-text" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~sampling:info_never ~retention:defer_completed ())
            text);
      make ~name:"core/retention/completion-inspect" ~suite:Core
        ~boundary:"sampling-completion-inspection" ~payload:"open-token"
        (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~sampling:info_never ~retention:inspect_completed ())
            redaction_token);
      make ~name:"core/retention/wide-rescue" ~suite:Core
        ~boundary:"sampling-wide-rescue" ~payload:"open-wide" (fun () ->
          core_unit_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~sampling:info_never ~retention:retain_completed ())
            open_wide);
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
      make ~name:"core/routing/unmatched" ~suite:Core
        ~boundary:"routing-unmatched" ~payload:"tagged-text" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ routed_drain (fun _ -> false) ]
               ())
            text);
      make ~name:"core/routing/matched" ~suite:Core ~boundary:"routing-matched"
        ~payload:"tagged-text" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ routed_drain (fun _ -> true) ]
               ())
            text);
      make ~name:"core/routing/multiple-matched" ~suite:Core
        ~boundary:"routing-multiple" ~payload:"tagged-text" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:
                 [
                   routed_drain (fun _ -> true);
                   routed_drain (fun _ -> true);
                   routed_drain (fun _ -> true);
                   routed_drain (fun _ -> true);
                 ]
               ())
            text);
      make ~name:"core/routing/stricter-redaction" ~suite:Core
        ~boundary:"routing-redaction" ~payload:"open-token" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ routed_stricter_drain (fun _ -> true) ]
               ())
            redaction_token);
      make ~name:"core/enrichment/zero" ~suite:Core ~boundary:"enrichment"
        ~payload:"zero-enrichers" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ())
            open_small);
      make ~name:"core/enrichment/one" ~suite:Core ~boundary:"enrichment"
        ~payload:"one-enricher" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~enrichers:[ benchmark_enricher_one ] ())
            open_small);
      make ~name:"core/enrichment/multiple" ~suite:Core ~boundary:"enrichment"
        ~payload:"two-enrichers" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~enrichers:[ benchmark_enricher_one; benchmark_enricher_two ]
               ())
            open_small);
      make ~name:"core/enrichment/many" ~suite:Core ~boundary:"enrichment"
        ~payload:"eight-enrichers" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~enrichers:benchmark_enrichers_many ())
            open_small);
      make ~name:"core/materialization/default" ~suite:Core
        ~boundary:"materialization" ~payload:"default-limits" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ())
            untyped_nested);
      make ~name:"core/materialization/depth" ~suite:Core
        ~boundary:"materialization-depth" ~payload:"depth-32" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ())
            (nested_structured 32));
      make ~name:"core/materialization/depth-truncated" ~suite:Core
        ~boundary:"materialization-depth" ~payload:"depth-32-limit-8" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~limits:p16_depth_limits ())
            (nested_structured 32));
      make ~name:"core/materialization/width" ~suite:Core
        ~boundary:"materialization-width" ~payload:"128-fields" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ())
            (wide_structured 128));
      make ~name:"core/materialization/width-truncated" ~suite:Core
        ~boundary:"materialization-width" ~payload:"128-fields-limit-32"
        (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~limits:p16_width_limits ())
            (wide_structured 128));
      make ~name:"core/materialization/collection" ~suite:Core
        ~boundary:"materialization-collection" ~payload:"128-items" (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ())
            (collection_structured 128));
      make ~name:"core/materialization/collection-truncated" ~suite:Core
        ~boundary:"materialization-collection" ~payload:"128-items-limit-32"
        (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~limits:p16_collection_limits ())
            (collection_structured 128));
      make ~name:"core/materialization/nodes-truncated" ~suite:Core
        ~boundary:"materialization-nodes" ~payload:"128-fields-limit-32"
        (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~limits:p16_node_limits ())
            (wide_structured 128));
      make ~name:"core/materialization/bytes-truncated" ~suite:Core
        ~boundary:"materialization-bytes" ~payload:"128-bytes-limit-64"
        (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~limits:p16_byte_limits ())
            (bytes_structured 128));
      make ~name:"core/materialization/truncated-string" ~suite:Core
        ~boundary:"materialization" ~payload:"localized-string-marker"
        (fun () ->
          core_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ~limits:p16_limits ())
            limited_string);
      make ~name:"core/materialization/full-withholding" ~suite:Core
        ~boundary:"materialization-total-bytes" ~payload:"one-byte-budget"
        (fun () -> withheld_core_operation p16_withholding_limits open_small);
      make ~name:"core/materialization/embedded-typed" ~suite:Core
        ~boundary:"materialization-embedded" ~payload:"ppx-embedded-record"
        (fun () ->
          core_unit_operation
            (config ~console:Observe.Config.Silent
               ~drains:[ accepted_drain () ]
               ())
            ppx_embedded_typed);
      make ~name:"core/canonical/typed-point" ~suite:Core
        ~boundary:"canonical-freeze" ~payload:"typed-small" (fun () ->
          retained_core_operation (fun () -> Observe.Logs.info (typed_small ())));
      make ~name:"core/canonical/open-point" ~suite:Core
        ~boundary:"open-fragment" ~payload:"open-small" (fun () ->
          retained_core_operation (fun () -> Observe.Logs.info (open_small ())));
      make ~name:"core/redaction/none" ~suite:Core ~boundary:"redaction"
        ~payload:"open-token" (fun () ->
          redaction_core_operation redaction_token);
      make ~name:"core/redaction/unmatched" ~suite:Core ~boundary:"redaction"
        ~payload:"open-token" (fun () ->
          redaction_core_operation ~redaction:redaction_unmatched
            redaction_token);
      make ~name:"core/redaction/unmatched-prefixes-256" ~suite:Core
        ~boundary:"redaction-scaling" ~payload:"open-token" (fun () ->
          redaction_core_operation ~redaction:redaction_unmatched_prefixes_256
            redaction_token);
      make ~name:"core/redaction/unmatched-contains-256" ~suite:Core
        ~boundary:"redaction-scaling" ~payload:"open-token" (fun () ->
          redaction_core_operation ~redaction:redaction_unmatched_contains_256
            redaction_token);
      make ~name:"core/redaction/unmatched-exact-256" ~suite:Core
        ~boundary:"redaction-scaling" ~payload:"open-token" (fun () ->
          redaction_core_operation ~redaction:redaction_unmatched_exact_256
            redaction_token);
      make ~name:"core/redaction/baseline-open-nested" ~suite:Core
        ~boundary:"redaction-baseline" ~payload:"open-nested" (fun () ->
          redaction_core_operation untyped_nested);
      make ~name:"core/redaction/exact-open-nested" ~suite:Core
        ~boundary:"redaction-exact" ~payload:"open-nested" (fun () ->
          redaction_core_operation ~redaction:redaction_exact_nested_remove
            untyped_nested);
      make ~name:"core/redaction/baseline-open-wide-nested" ~suite:Core
        ~boundary:"redaction-baseline-wide" ~payload:"open-nested-wide"
        (fun () -> redaction_core_unit_operation redaction_open_wide_nested);
      make ~name:"core/redaction/exact-open-wide-nested" ~suite:Core
        ~boundary:"redaction-exact-wide" ~payload:"open-nested-wide" (fun () ->
          redaction_core_unit_operation ~redaction:redaction_exact_nested_remove
            redaction_open_wide_nested);
      make ~name:"core/redaction/baseline-typed-point" ~suite:Core
        ~boundary:"redaction-baseline-typed" ~payload:"typed-nested" (fun () ->
          redaction_core_operation typed_nested);
      make ~name:"core/redaction/exact-typed-point" ~suite:Core
        ~boundary:"redaction-exact-typed" ~payload:"typed-nested" (fun () ->
          redaction_core_operation
            ~redaction:redaction_exact_typed_nested_remove typed_nested);
      make ~name:"core/redaction/baseline-typed-wide" ~suite:Core
        ~boundary:"redaction-baseline-typed" ~payload:"typed-nested-wide"
        (fun () -> redaction_core_unit_operation nested_typed_wide);
      make ~name:"core/redaction/exact-typed-wide" ~suite:Core
        ~boundary:"redaction-exact-typed" ~payload:"typed-nested-wide"
        (fun () ->
          redaction_core_unit_operation
            ~redaction:redaction_exact_typed_nested_remove nested_typed_wide);
      make ~name:"core/redaction/structured-matcher" ~suite:Core
        ~boundary:"redaction-matcher" ~payload:"open-token" (fun () ->
          redaction_core_operation ~redaction:redaction_matching_replace
            redaction_token);
      make ~name:"core/redaction/structured-matcher-128" ~suite:Core
        ~boundary:"redaction-matcher-scaling" ~payload:"open-token-list"
        (fun () ->
          redaction_core_operation ~redaction:redaction_matching_replace
            (redaction_tokens 128));
      make ~name:"core/redaction/point-text" ~suite:Core
        ~boundary:"redaction-text" ~payload:"point-text" (fun () ->
          redaction_core_operation ~redaction:redaction_matching_replace
            redaction_text);
      make ~name:"core/redaction/wide-annotation" ~suite:Core
        ~boundary:"redaction-annotation" ~payload:"wide-annotation" (fun () ->
          redaction_core_unit_operation ~redaction:redaction_matching_replace
            redaction_annotation);
      make ~name:"core/redaction/finite-mask" ~suite:Core
        ~boundary:"redaction-mask" ~payload:"open-token" (fun () ->
          redaction_core_operation ~redaction:redaction_finite_mask
            redaction_token);
      make ~name:"core/redaction/custom-mask" ~suite:Core
        ~boundary:"redaction-mask" ~payload:"open-token" (fun () ->
          redaction_core_operation ~redaction:redaction_custom_mask
            redaction_token);
      make ~name:"core/redaction/custom-mask-failure" ~suite:Core
        ~boundary:"redaction-mask" ~payload:"open-token" (fun () ->
          redaction_core_operation ~redaction:redaction_failing_custom_mask
            redaction_token);
      make ~name:"core/redaction/stricter-drain" ~suite:Core
        ~boundary:"redaction-drain" ~payload:"open-token" (fun () ->
          redaction_stricter_drain_operation redaction_token);
      make ~name:"core/wide/open-fragment" ~suite:Core
        ~boundary:"open-wide-fragment" ~payload:"open-small" (fun () ->
          retained_wide_operation open_wide);
      make ~name:"core/wide-stage/open-create" ~suite:Core
        ~boundary:"wide-create" ~payload:"open-empty" (fun () ->
          retained_wide_operation open_wide_create);
      make ~name:"core/wide-stage/open-create-set" ~suite:Core
        ~boundary:"wide-create-set" ~payload:"open-small" (fun () ->
          retained_wide_operation open_wide_create_set);
      make ~name:"core/wide-stage/open-create-emit" ~suite:Core
        ~boundary:"wide-create-emit" ~payload:"open-empty" (fun () ->
          retained_wide_operation open_wide_create_emit);
      make ~name:"core/wide-stage/open-repeated" ~suite:Core
        ~boundary:"wide-repeated" ~payload:"open-four-updates" (fun () ->
          retained_wide_operation open_wide_repeated);
      make ~name:"core/wide/typed-patch" ~suite:Core
        ~boundary:"typed-wide-patch" ~payload:"typed-small" (fun () ->
          retained_wide_operation typed_wide);
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
        ~payload:"open-error" (fun () ->
          retained_wide_operation open_wide_error);
      make ~name:"core/wide-stage/open-set-level" ~suite:Core
        ~boundary:"wide-level" ~payload:"open-empty" (fun () ->
          retained_wide_operation open_wide_set_level);
      make ~name:"core/wide-stage/open-annotate" ~suite:Core
        ~boundary:"wide-annotation" ~payload:"one-warning" (fun () ->
          retained_wide_operation open_wide_annotate);
      make ~name:"core/correlation/explicit-point" ~suite:Core
        ~boundary:"correlated-point" ~payload:"tagged-text" (fun () ->
          retained_correlated_point_operation ());
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
          retained_core_with_observer (fun observer ->
              operation_success observer));
      make ~name:"core/operation/failure" ~suite:Core
        ~boundary:"operation-failure" ~payload:"open-error" (fun () ->
          retained_core_with_observer (fun observer ->
              operation_failure observer));
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
          core_operation
            (config ~console:Observe.Config.Ndjson ())
            untyped_small);
      make ~name:"core/json/typed-small" ~suite:Core ~boundary:"json"
        ~payload:"typed-small" (fun () ->
          core_operation (config ~console:Observe.Config.Ndjson ()) typed_small);
      make ~name:"core/json/untyped-nested" ~suite:Core ~boundary:"json"
        ~payload:"untyped-nested" (fun () ->
          core_operation
            (config ~console:Observe.Config.Ndjson ())
            untyped_nested);
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
  in
  let p16, established =
    List.partition
      (fun scenario ->
        String.starts_with ~prefix:"core/enrichment/" scenario.name
        || String.starts_with ~prefix:"core/materialization/" scenario.name)
      scenarios
  in
  established @ p16

let lwt_unix_scenarios =
  [
    make ~name:"lwt-unix/lifecycle/flush-empty" ~suite:Lwt_unix
      ~boundary:"lifecycle-flush" ~payload:"no-output" (fun () ->
        prepare_lifecycle_flush ());
    make ~name:"lwt-unix/lifecycle/flush-one-output" ~suite:Lwt_unix
      ~boundary:"lifecycle-flush" ~payload:"one-output" (fun () ->
        prepare_lifecycle_flush
          ~facts:(fun () -> Observe_lwt_unix.Lifecycle.Integration.No_problem)
          ());
    make ~name:"lwt-unix/lifecycle/flush-reported-loss" ~suite:Lwt_unix
      ~boundary:"lifecycle-report" ~payload:"rejected-and-lost" (fun () ->
        prepare_lifecycle_flush
          ~facts:(fun () ->
            Observe_lwt_unix.Lifecycle.Integration.Rejected_and_lost)
          ());
    make ~name:"lwt-unix/lifecycle/shutdown-settled" ~suite:Lwt_unix
      ~boundary:"lifecycle-shutdown-idempotent" ~payload:"no-output" (fun () ->
        prepare_settled_shutdown ());
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
    make ~logical_operations:9 ~name:"lwt-unix/sampling/stable-fan-in"
      ~suite:Lwt_unix ~boundary:"stable-sampling" ~payload:"nine-related-logs"
      (fun () ->
        prepare_lwt_unix_operation
          ~sampling_draw:(fun () -> 0.25)
          ~flush:false
          (config ~console:Observe.Config.Silent
             ~drains:[ accepted_drain () ]
             ~sampling:
               (Observe.Logs.Sampling.create
                  ~info:(Observe.Logs.Sampling.Rate.percent_exn 50.)
                  ~stability:Observe.Logs.Sampling.Correlation_stable ())
             ())
          lwt_stable_fan_in);
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
  @ fs_scenarios
  @ lwt_unix_scenarios
  @ fs_lwt_unix_scenarios

let find wanted = List.find_opt (fun scenario -> scenario.name = wanted) all

let with_operation scenario callback =
  let prepared = scenario.prepare () in
  Fun.protect ~finally:prepared.cleanup (fun () ->
      callback prepared.operation prepared.retained_bytes prepared.encoded_bytes)
