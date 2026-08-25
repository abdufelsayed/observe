let output_path = ref ".logs/benchmarks/observe-benchmark.json"
let suite = ref "all"
let quota_ms = ref 500
let limit = ref 2_000
let allocation_runs = ref 10_000

let commit =
  ref (Option.value (Sys.getenv_opt "GITHUB_SHA") ~default:"working-tree")

let baseline = ref None
let worker = ref None
let scenario_filter = ref None
let list_scenarios = ref false

let options =
  [
    ( "--output",
      Arg.Set_string output_path,
      "PATH Write the structured benchmark report to PATH" );
    ( "--suite",
      Arg.Set_string suite,
      "SUITE Run all, component, core, fs, lwt-unix, or fs-lwt-unix scenarios"
    );
    ( "--quota-ms",
      Arg.Set_int quota_ms,
      "MILLISECONDS Set Bechamel's sampling quota per scenario" );
    ( "--limit",
      Arg.Set_int limit,
      "SAMPLES Set Bechamel's maximum sample count per scenario" );
    ( "--allocation-runs",
      Arg.Set_int allocation_runs,
      "RUNS Set the fixed batch size for GC allocation counters" );
    ( "--commit",
      Arg.Set_string commit,
      "REVISION Record REVISION in report metadata" );
    ( "--compare",
      Arg.String (fun path -> baseline := Some path),
      "PATH Compare the new measurements with a prior JSON report" );
    ( "--scenario",
      Arg.String (fun name -> scenario_filter := Some name),
      "NAME Run one named scenario" );
    ( "--list",
      Arg.Set list_scenarios,
      " List available scenarios without running them" );
    ( "--worker",
      Arg.String (fun name -> worker := Some name),
      "SCENARIO Internal worker mode" );
  ]

let usage = "observe_bench [OPTIONS]"

let fail message =
  prerr_endline ("observe-bench: " ^ message);
  exit 2

let selected_scenarios () =
  let matches scenario =
    match !suite with
    | "all" -> true
    | "component" -> Scenario.suite scenario = Scenario.Component
    | "core" -> Scenario.suite scenario = Scenario.Core
    | "fs" -> Scenario.suite scenario = Scenario.Fs
    | "lwt-unix" -> Scenario.suite scenario = Scenario.Lwt_unix
    | "fs-lwt-unix" -> Scenario.suite scenario = Scenario.Fs_lwt_unix
    | value -> fail ("unknown suite: " ^ value)
  in
  let matches_name scenario =
    Option.fold ~none:true
      ~some:(fun name -> String.equal name (Scenario.name scenario))
      !scenario_filter
  in
  let selected =
    List.filter
      (fun scenario -> matches scenario && matches_name scenario)
      Scenario.all
  in
  if Option.is_some !scenario_filter && selected = [] then
    fail ("unknown scenario: " ^ Option.get !scenario_filter)
  else selected

let process_succeeded = function Unix.WEXITED 0 -> true | _ -> false

let require_finite_nonnegative scenario field value =
  if (not (Float.is_finite value)) || value < 0. then
    fail
      (Format.asprintf "scenario %s returned invalid %s: %g"
         (Scenario.name scenario) field value)

let validate_optional_measure scenario field = function
  | None -> ()
  | Some value -> require_finite_nonnegative scenario field value

let validate_measurement scenario (measurement : Measurement.t) =
  let expected_name = Scenario.name scenario in
  let expected_suite = Scenario.suite_name (Scenario.suite scenario) in
  if not (String.equal measurement.name expected_name) then
    fail
      (Format.asprintf "scenario %s returned measurement for %s" expected_name
         measurement.name);
  if not (String.equal measurement.suite expected_suite) then
    fail
      (Format.asprintf "scenario %s returned suite %s; expected %s"
         expected_name measurement.suite expected_suite);
  if not (String.equal measurement.boundary (Scenario.boundary scenario)) then
    fail
      (Format.asprintf "scenario %s returned the wrong boundary" expected_name);
  if not (String.equal measurement.payload (Scenario.payload scenario)) then
    fail
      (Format.asprintf "scenario %s returned the wrong payload" expected_name);
  require_finite_nonnegative scenario "nanoseconds_per_operation"
    measurement.nanoseconds_per_operation;
  if measurement.nanoseconds_per_operation = 0. then
    fail (Format.asprintf "scenario %s returned zero latency" expected_name);
  require_finite_nonnegative scenario "operations_per_second"
    measurement.operations_per_second;
  require_finite_nonnegative scenario "minor_bytes_per_operation"
    measurement.minor_bytes_per_operation;
  require_finite_nonnegative scenario "major_bytes_per_operation"
    measurement.major_bytes_per_operation;
  require_finite_nonnegative scenario "promoted_bytes_per_operation"
    measurement.promoted_bytes_per_operation;
  require_finite_nonnegative scenario "minor_collections_per_operation"
    measurement.minor_collections_per_operation;
  require_finite_nonnegative scenario "major_collections_per_operation"
    measurement.major_collections_per_operation;
  validate_optional_measure scenario "retained_bytes" measurement.retained_bytes;
  validate_optional_measure scenario "encoded_bytes" measurement.encoded_bytes;
  Option.iter
    (fun value ->
      if not (Float.is_finite value) then
        fail
          (Format.asprintf "scenario %s returned invalid r_squared: %g"
             expected_name value))
    measurement.r_squared;
  if measurement.samples <= 0 then
    fail (Format.asprintf "scenario %s returned no samples" expected_name);
  if Int64.compare measurement.measured_nanoseconds 0L <= 0 then
    fail (Format.asprintf "scenario %s returned no measured time" expected_name);
  measurement

let run_worker configuration scenario =
  let arguments =
    [|
      Sys.executable_name;
      "--worker";
      Scenario.name scenario;
      "--quota-ms";
      string_of_int configuration.Measurement.quota_ms;
      "--limit";
      string_of_int configuration.limit;
      "--allocation-runs";
      string_of_int configuration.allocation_runs;
    |]
  in
  let channel = Unix.open_process_args_in Sys.executable_name arguments in
  let decoded =
    try Some (Marshal.from_channel channel : Measurement.t)
    with End_of_file -> None
  in
  let status = Unix.close_process_in channel in
  match (status, decoded) with
  | status, Some result when process_succeeded status ->
      validate_measurement scenario result
  | Unix.WEXITED 0, None ->
      fail ("scenario " ^ Scenario.name scenario ^ " returned no measurement")
  | Unix.WEXITED code, _ ->
      fail
        (Format.asprintf "scenario %s exited with status %d"
           (Scenario.name scenario) code)
  | Unix.WSIGNALED signal, _ ->
      fail
        (Format.asprintf "scenario %s received signal %d"
           (Scenario.name scenario) signal)
  | Unix.WSTOPPED signal, _ ->
      fail
        (Format.asprintf "scenario %s stopped with signal %d"
           (Scenario.name scenario) signal)

let run_one configuration name =
  match Scenario.find name with
  | None -> fail ("unknown worker scenario: " ^ name)
  | Some scenario ->
      let result =
        Scenario.with_operation scenario
          (Measurement.run configuration scenario)
      in
      Marshal.to_channel stdout result [ Marshal.No_sharing ];
      flush stdout

let () =
  Arg.parse options
    (fun argument -> fail ("unexpected argument: " ^ argument))
    usage;
  if !quota_ms <= 0 then fail "--quota-ms must be a positive integer";
  if !limit <= 1 then fail "--limit must be greater than one";
  if !allocation_runs <= 0 then
    fail "--allocation-runs must be a positive integer";
  let configuration =
    Measurement.
      {
        quota_ms = !quota_ms;
        limit = !limit;
        allocation_runs = !allocation_runs;
      }
  in
  match !worker with
  | Some name -> run_one configuration name
  | None ->
      let scenarios = selected_scenarios () in
      if !list_scenarios then
        List.iter
          (fun scenario ->
            Format.printf "%s\t%s\t%s\t%s\n" (Scenario.name scenario)
              (Scenario.suite_name (Scenario.suite scenario))
              (Scenario.boundary scenario)
              (Scenario.payload scenario))
          scenarios
      else
        let measurements = List.map (run_worker configuration) scenarios in
        Report.print_table measurements;
        let metadata =
          Report.metadata ~commit:!commit ~suite:!suite configuration
        in
        Report.write_json ~path:!output_path metadata measurements;
        Format.printf "\nWrote %s\n%!" !output_path;
        Option.iter
          (fun path -> Report.print_comparison ~baseline:path measurements)
          !baseline
