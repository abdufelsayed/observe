type central_measurement = {
  nanoseconds_per_operation : float;
  operations_per_second : float;
  minor_bytes_per_operation : float;
  major_bytes_per_operation : float;
  promoted_bytes_per_operation : float;
  retained_bytes : float option;
  encoded_bytes : float option;
  minor_collections_per_operation : float;
  major_collections_per_operation : float;
  r_squared : float option;
  samples : int;
  measured_nanoseconds : int64;
}
[@@deriving observe]

type spread = {
  latency_min_nanoseconds : float;
  latency_max_nanoseconds : float;
  latency_standard_deviation : float;
  minor_bytes_min : float;
  minor_bytes_max : float;
  major_bytes_min : float;
  major_bytes_max : float;
  promoted_bytes_min : float;
  promoted_bytes_max : float;
  retained_bytes_min : float option;
  retained_bytes_max : float option;
  encoded_bytes_min : float option;
  encoded_bytes_max : float option;
}
[@@deriving observe]

type entry = {
  name : string;
  suite : string;
  boundary : string;
  payload : string;
  logical_operations : int;
  measurement : central_measurement;
  spread : spread;
  repetitions : int;
}
[@@deriving observe]

type metadata = {
  generated_at_utc : string;
  commit : string;
  suite : string;
  benchmark_engine : string;
  benchmark_engine_version : string;
  ocaml_version : string;
  operating_system : string;
  architecture : string;
  runner_name : string option;
  runner_image : string option;
  github_run_id : string option;
  quota_ms : int;
  sample_limit : int;
  allocation_runs : int;
  repetitions : int;
  word_size_bits : int;
}
[@@deriving observe]

type report = {
  schema_version : int;
  metadata : metadata;
  results : entry list;
}
[@@deriving observe]

type summary = {
  median : Measurement.t;
  latency_min_nanoseconds : float;
  latency_max_nanoseconds : float;
  latency_standard_deviation : float;
  minor_bytes_min : float;
  minor_bytes_max : float;
  major_bytes_min : float;
  major_bytes_max : float;
  promoted_bytes_min : float;
  promoted_bytes_max : float;
  retained_bytes_min : float option;
  retained_bytes_max : float option;
  encoded_bytes_min : float option;
  encoded_bytes_max : float option;
  repetitions : int;
}

let optional_environment name =
  match Sys.getenv_opt name with
  | None | Some "" -> None
  | Some value -> Some value

let command_first_line command arguments =
  try
    let channel = Unix.open_process_args_in command arguments in
    match input_line channel with
    | line -> (
        match Unix.close_process_in channel with
        | Unix.WEXITED 0 -> line
        | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> "unknown")
    | exception _ ->
        ignore (Unix.close_process_in channel : Unix.process_status);
        "unknown"
  with _ -> "unknown"

let architecture () =
  match optional_environment "RUNNER_ARCH" with
  | Some value -> value
  | None -> command_first_line "uname" [| "uname"; "-m" |]

let timestamp () =
  let time = Unix.gmtime (Unix.gettimeofday ()) in
  Format.asprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (time.tm_year + 1900)
    (time.tm_mon + 1) time.tm_mday time.tm_hour time.tm_min time.tm_sec

let metadata ~commit ~suite ~repetitions
    (configuration : Measurement.configuration) =
  {
    generated_at_utc = timestamp ();
    commit;
    suite;
    benchmark_engine = "bechamel";
    benchmark_engine_version =
      command_first_line "opam"
        [|
          "opam";
          "list";
          "--installed";
          "--short";
          "--columns=version";
          "bechamel";
        |];
    ocaml_version = Sys.ocaml_version;
    operating_system =
      Option.value
        (optional_environment "RUNNER_OS")
        ~default:(command_first_line "uname" [| "uname"; "-s" |]);
    architecture = architecture ();
    runner_name = optional_environment "RUNNER_NAME";
    runner_image = optional_environment "ImageOS";
    github_run_id = optional_environment "GITHUB_RUN_ID";
    quota_ms = configuration.quota_ms;
    sample_limit = configuration.limit;
    allocation_runs = configuration.allocation_runs;
    repetitions;
    word_size_bits = Sys.word_size;
  }

let sorted values = List.sort Float.compare values

let median values =
  match sorted values with
  | [] -> invalid_arg "Report.median: empty input"
  | values ->
      let length = List.length values in
      let middle = length / 2 in
      if length mod 2 = 1 then List.nth values middle
      else (List.nth values (middle - 1) +. List.nth values middle) /. 2.

let rec present_values = function
  | [] -> Some []
  | None :: _ -> None
  | Some value :: rest ->
      Option.map (fun values -> value :: values) (present_values rest)

let median_optional values = Option.map median (present_values values)

let range values =
  ( List.fold_left Float.min Float.infinity values,
    List.fold_left Float.max Float.neg_infinity values )

let optional_range values =
  match present_values values with
  | None | Some [] -> (None, None)
  | Some values ->
      let minimum, maximum = range values in
      (Some minimum, Some maximum)

let standard_deviation values =
  let count = float_of_int (List.length values) in
  let mean = List.fold_left ( +. ) 0. values /. count in
  let squared_difference value =
    let difference = value -. mean in
    difference *. difference
  in
  sqrt
    (List.fold_left
       (fun total value -> total +. squared_difference value)
       0. values
    /. count)

let summarize measurements =
  match measurements with
  | [] -> invalid_arg "Report.summarize: empty input"
  | (first : Measurement.t) :: _ ->
      List.iter
        (fun (measurement : Measurement.t) ->
          if
            not
              (String.equal first.name measurement.name
              && String.equal first.suite measurement.suite
              && String.equal first.boundary measurement.boundary
              && String.equal first.payload measurement.payload
              && Int.equal first.logical_operations
                   measurement.logical_operations)
          then invalid_arg "Report.summarize: mixed benchmark scenarios")
        measurements;
      let values project = List.map project measurements in
      let nanoseconds_per_operation =
        median
          (values (fun measurement -> measurement.nanoseconds_per_operation))
      in
      let median =
        Measurement.
          {
            name = first.name;
            suite = first.suite;
            boundary = first.boundary;
            payload = first.payload;
            logical_operations = first.logical_operations;
            nanoseconds_per_operation;
            operations_per_second = 1_000_000_000. /. nanoseconds_per_operation;
            minor_bytes_per_operation =
              median
                (values (fun measurement ->
                     measurement.minor_bytes_per_operation));
            major_bytes_per_operation =
              median
                (values (fun measurement ->
                     measurement.major_bytes_per_operation));
            promoted_bytes_per_operation =
              median
                (values (fun measurement ->
                     measurement.promoted_bytes_per_operation));
            retained_bytes =
              median_optional
                (values (fun measurement -> measurement.retained_bytes));
            encoded_bytes =
              median_optional
                (values (fun measurement -> measurement.encoded_bytes));
            minor_collections_per_operation =
              median
                (values (fun measurement ->
                     measurement.minor_collections_per_operation));
            major_collections_per_operation =
              median
                (values (fun measurement ->
                     measurement.major_collections_per_operation));
            r_squared =
              median_optional
                (values (fun measurement -> measurement.r_squared));
            samples =
              List.fold_left
                (fun total (measurement : Measurement.t) ->
                  total + measurement.samples)
                0 measurements;
            measured_nanoseconds =
              List.fold_left
                (fun total (measurement : Measurement.t) ->
                  Int64.add total measurement.measured_nanoseconds)
                0L measurements;
          }
      in
      let latencies =
        values (fun measurement -> measurement.nanoseconds_per_operation)
      in
      let latency_min_nanoseconds, latency_max_nanoseconds = range latencies in
      let minor_bytes =
        values (fun measurement -> measurement.minor_bytes_per_operation)
      in
      let major_bytes =
        values (fun measurement -> measurement.major_bytes_per_operation)
      in
      let promoted_bytes =
        values (fun measurement -> measurement.promoted_bytes_per_operation)
      in
      let minor_bytes_min, minor_bytes_max = range minor_bytes in
      let major_bytes_min, major_bytes_max = range major_bytes in
      let promoted_bytes_min, promoted_bytes_max = range promoted_bytes in
      let retained_bytes_min, retained_bytes_max =
        optional_range (values (fun measurement -> measurement.retained_bytes))
      in
      let encoded_bytes_min, encoded_bytes_max =
        optional_range (values (fun measurement -> measurement.encoded_bytes))
      in
      {
        median;
        latency_min_nanoseconds;
        latency_max_nanoseconds;
        latency_standard_deviation = standard_deviation latencies;
        minor_bytes_min;
        minor_bytes_max;
        major_bytes_min;
        major_bytes_max;
        promoted_bytes_min;
        promoted_bytes_max;
        retained_bytes_min;
        retained_bytes_max;
        encoded_bytes_min;
        encoded_bytes_max;
        repetitions = List.length measurements;
      }

let entry summary =
  let measurement = summary.median in
  {
    name = measurement.name;
    suite = measurement.suite;
    boundary = measurement.boundary;
    payload = measurement.payload;
    logical_operations = measurement.logical_operations;
    measurement =
      {
        nanoseconds_per_operation = measurement.nanoseconds_per_operation;
        operations_per_second = measurement.operations_per_second;
        minor_bytes_per_operation = measurement.minor_bytes_per_operation;
        major_bytes_per_operation = measurement.major_bytes_per_operation;
        promoted_bytes_per_operation = measurement.promoted_bytes_per_operation;
        retained_bytes = measurement.retained_bytes;
        encoded_bytes = measurement.encoded_bytes;
        minor_collections_per_operation =
          measurement.minor_collections_per_operation;
        major_collections_per_operation =
          measurement.major_collections_per_operation;
        r_squared = measurement.r_squared;
        samples = measurement.samples;
        measured_nanoseconds = measurement.measured_nanoseconds;
      };
    spread =
      {
        latency_min_nanoseconds = summary.latency_min_nanoseconds;
        latency_max_nanoseconds = summary.latency_max_nanoseconds;
        latency_standard_deviation = summary.latency_standard_deviation;
        minor_bytes_min = summary.minor_bytes_min;
        minor_bytes_max = summary.minor_bytes_max;
        major_bytes_min = summary.major_bytes_min;
        major_bytes_max = summary.major_bytes_max;
        promoted_bytes_min = summary.promoted_bytes_min;
        promoted_bytes_max = summary.promoted_bytes_max;
        retained_bytes_min = summary.retained_bytes_min;
        retained_bytes_max = summary.retained_bytes_max;
        encoded_bytes_min = summary.encoded_bytes_min;
        encoded_bytes_max = summary.encoded_bytes_max;
      };
    repetitions = summary.repetitions;
  }

let rec ensure_directory path =
  if path = "." || path = "/" || Sys.file_exists path then ()
  else (
    ensure_directory (Filename.dirname path);
    Unix.mkdir path 0o755)

let write_file path contents =
  ensure_directory (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () ->
      output_string channel contents;
      output_char channel '\n')

let write_json ~path metadata summaries =
  let report : report =
    { schema_version = 4; metadata; results = List.map entry summaries }
  in
  write_file path (Observe.Type.to_json_string report_t report)

let print_table summaries =
  Format.printf "%-43s %12s %25s %12s %12s %12s %12s %7s\n%!" "scenario"
    "median ns/op" "range ns/op" "minor B/op" "major B/op" "retained B"
    "encoded B" "runs";
  List.iter
    (fun summary ->
      let measurement = summary.median in
      Format.printf
        "%-43s %12.1f %12.1f..%-12.1f %12.1f %12.1f %12s %12s %7d\n%!"
        measurement.name measurement.nanoseconds_per_operation
        summary.latency_min_nanoseconds summary.latency_max_nanoseconds
        measurement.minor_bytes_per_operation
        measurement.major_bytes_per_operation
        (match measurement.retained_bytes with
        | None -> "n/a"
        | Some bytes -> Format.asprintf "%.0f" bytes)
        (match measurement.encoded_bytes with
        | None -> "n/a"
        | Some bytes -> Format.asprintf "%.0f" bytes)
        summary.repetitions)
    summaries

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

type legacy_baseline_entry = {
  name : string;
  suite : string;
  boundary : string;
  payload : string;
  nanoseconds_per_operation : float;
  operations_per_second : float;
  minor_bytes_per_operation : float;
  major_bytes_per_operation : float;
  promoted_bytes_per_operation : float;
  retained_bytes : float option;
  encoded_bytes : float option;
  minor_collections_per_operation : float;
  major_collections_per_operation : float;
  r_squared : float option;
  samples : int;
  measured_nanoseconds : int64;
}
[@@deriving observe]

type baseline_metadata = {
  benchmark_engine : string;
  benchmark_engine_version : string;
  ocaml_version : string;
  operating_system : string;
  architecture : string;
  quota_ms : int;
  sample_limit : int;
  allocation_runs : int;
  word_size_bits : int;
}
[@@deriving observe]

type baseline_report = {
  schema_version : int;
  metadata : baseline_metadata;
  results : legacy_baseline_entry list;
}
[@@deriving observe]

type scenario_identity = {
  name : string;
  suite : string option;
  boundary : string option;
  payload : string option;
  logical_operations : int option;
}

type comparable_measurement = {
  latency_nanoseconds : float option;
  minor_bytes_per_operation : float option;
  major_bytes_per_operation : float option;
  promoted_bytes_per_operation : float option;
  retained_bytes : float option;
  encoded_bytes : float option;
}

type baseline_entry = {
  identity : scenario_identity;
  measurement : comparable_measurement;
  legacy : bool;
}

type baseline = {
  metadata : baseline_metadata;
  results : baseline_entry list;
  legacy : bool;
}

let baseline_metadata (metadata : metadata) =
  {
    benchmark_engine = metadata.benchmark_engine;
    benchmark_engine_version = metadata.benchmark_engine_version;
    ocaml_version = metadata.ocaml_version;
    operating_system = metadata.operating_system;
    architecture = metadata.architecture;
    quota_ms = metadata.quota_ms;
    sample_limit = metadata.sample_limit;
    allocation_runs = metadata.allocation_runs;
    word_size_bits = metadata.word_size_bits;
  }

let schema4_entry (entry : entry) =
  {
    identity =
      {
        name = entry.name;
        suite = Some entry.suite;
        boundary = Some entry.boundary;
        payload = Some entry.payload;
        logical_operations = Some entry.logical_operations;
      };
    measurement =
      {
        latency_nanoseconds = Some entry.measurement.nanoseconds_per_operation;
        minor_bytes_per_operation =
          Some entry.measurement.minor_bytes_per_operation;
        major_bytes_per_operation =
          Some entry.measurement.major_bytes_per_operation;
        promoted_bytes_per_operation =
          Some entry.measurement.promoted_bytes_per_operation;
        retained_bytes = entry.measurement.retained_bytes;
        encoded_bytes = entry.measurement.encoded_bytes;
      };
    legacy = false;
  }

let legacy_entry (entry : legacy_baseline_entry) =
  {
    identity =
      {
        name = entry.name;
        suite = Some entry.suite;
        boundary = Some entry.boundary;
        payload = Some entry.payload;
        logical_operations = None;
      };
    measurement =
      {
        latency_nanoseconds = Some entry.nanoseconds_per_operation;
        minor_bytes_per_operation = Some entry.minor_bytes_per_operation;
        major_bytes_per_operation = Some entry.major_bytes_per_operation;
        promoted_bytes_per_operation = Some entry.promoted_bytes_per_operation;
        retained_bytes = entry.retained_bytes;
        encoded_bytes = entry.encoded_bytes;
      };
    legacy = true;
  }

let fail_duplicate_scenario source name =
  failwith (Format.asprintf "duplicate benchmark scenario %s in %s" name source)

let validate_unique_baseline_entries source entries =
  let seen = Hashtbl.create (List.length entries) in
  List.iter
    (fun (entry : baseline_entry) ->
      if Hashtbl.mem seen entry.identity.name then
        fail_duplicate_scenario source entry.identity.name;
      Hashtbl.add seen entry.identity.name ())
    entries

let read_baseline path =
  let contents = read_file path in
  match Observe.Type.of_json_string report_t contents with
  | Ok report when report.schema_version = 4 ->
      let results = List.map schema4_entry report.results in
      validate_unique_baseline_entries path results;
      { metadata = baseline_metadata report.metadata; results; legacy = false }
  | Ok report ->
      failwith
        (Format.asprintf "unsupported benchmark report schema %d in %s"
           report.schema_version path)
  | Error _ -> (
      match Observe.Type.of_json_string baseline_report_t contents with
      | Ok report ->
          if report.schema_version <> 3 then
            failwith
              (Format.asprintf "unsupported benchmark report schema %d in %s"
                 report.schema_version path);
          let results = List.map legacy_entry report.results in
          validate_unique_baseline_entries path results;
          { metadata = report.metadata; results; legacy = true }
      | Error (`Msg message) -> failwith ("invalid benchmark report: " ^ message)
      )

let require_compatible current previous =
  let mismatch field current previous =
    failwith
      (Format.asprintf
         "incompatible benchmark metadata for %s: current=%s baseline=%s" field
         current previous)
  in
  let strings field project =
    let current = project current and previous = project previous in
    if not (String.equal current previous) then mismatch field current previous
  in
  let integers field project =
    let current = project current and previous = project previous in
    if not (Int.equal current previous) then
      mismatch field (string_of_int current) (string_of_int previous)
  in
  strings "benchmark_engine" (fun value -> value.benchmark_engine);
  strings "benchmark_engine_version" (fun value ->
      value.benchmark_engine_version);
  strings "ocaml_version" (fun value -> value.ocaml_version);
  strings "operating_system" (fun value -> value.operating_system);
  strings "architecture" (fun value -> value.architecture);
  integers "quota_ms" (fun value -> value.quota_ms);
  integers "sample_limit" (fun value -> value.sample_limit);
  integers "allocation_runs" (fun value -> value.allocation_runs);
  integers "word_size_bits" (fun value -> value.word_size_bits)

let percentage_delta ~before ~after =
  if before = 0. then None else Some (((after /. before) -. 1.) *. 100.)

let require_scenario_identity current previous =
  let mismatch field current_value previous_value =
    failwith
      (Format.asprintf
         "incompatible scenario identity for %s (%s): current=%s baseline=%s"
         current.name field current_value previous_value)
  in
  let optional_string field current = function
    | None -> ()
    | Some previous when String.equal current previous -> ()
    | Some previous -> mismatch field current previous
  in
  let optional_int field current = function
    | None -> ()
    | Some previous when Int.equal current previous -> ()
    | Some previous ->
        mismatch field (string_of_int current) (string_of_int previous)
  in
  optional_string "suite" (Option.get current.suite) previous.suite;
  optional_string "boundary" (Option.get current.boundary) previous.boundary;
  optional_string "payload" (Option.get current.payload) previous.payload;
  optional_int "logical_operations"
    (Option.get current.logical_operations)
    previous.logical_operations

let comparable_of_summary summary =
  let measurement = summary.median in
  ( {
      name = measurement.name;
      suite = Some measurement.suite;
      boundary = Some measurement.boundary;
      payload = Some measurement.payload;
      logical_operations = Some measurement.logical_operations;
    },
    {
      latency_nanoseconds = Some measurement.nanoseconds_per_operation;
      minor_bytes_per_operation = Some measurement.minor_bytes_per_operation;
      major_bytes_per_operation = Some measurement.major_bytes_per_operation;
      promoted_bytes_per_operation =
        Some measurement.promoted_bytes_per_operation;
      retained_bytes = measurement.retained_bytes;
      encoded_bytes = measurement.encoded_bytes;
    } )

let baseline_median project entries =
  median_optional (List.map (fun entry -> project entry.measurement) entries)

let format_measurement = function
  | None -> "n/a"
  | Some value -> Format.asprintf "%.1f" value

let format_delta before after =
  match (before, after) with
  | Some before, Some after -> (
      match percentage_delta ~before ~after with
      | None -> "n/a"
      | Some delta -> Format.asprintf "%+.1f%%" delta)
  | None, _ | _, None -> "n/a"

let print_metric scenario metric before after =
  Format.printf "%-43s %-13s %14s %14s %10s\n%!" scenario metric
    (format_measurement before)
    (format_measurement after)
    (format_delta before after)

let print_comparison ~metadata ~baselines ~allow_scenario_drift summaries =
  let baseline_reports = List.map read_baseline baselines in
  List.iter
    (fun baseline_report ->
      require_compatible (baseline_metadata metadata) baseline_report.metadata)
    baseline_reports;
  let baseline_by_name = Hashtbl.create (List.length summaries) in
  List.iter
    (fun (baseline_report : baseline) ->
      List.iter
        (fun (entry : baseline_entry) ->
          Hashtbl.replace baseline_by_name entry.identity.name
            (entry
            :: Option.value
                 (Hashtbl.find_opt baseline_by_name entry.identity.name)
                 ~default:[]))
        baseline_report.results)
    baseline_reports;
  Hashtbl.iter
    (fun name entries ->
      if List.length entries <> List.length baseline_reports then
        failwith
          (Format.asprintf
             "baseline reports have inconsistent scenario sets at %s" name))
    baseline_by_name;
  let current_by_name = Hashtbl.create (List.length summaries) in
  List.iter
    (fun summary ->
      let identity, _ = comparable_of_summary summary in
      if Hashtbl.mem current_by_name identity.name then
        fail_duplicate_scenario "current measurements" identity.name;
      Hashtbl.add current_by_name identity.name ())
    summaries;
  let current_only =
    List.filter_map
      (fun summary ->
        let identity, _ = comparable_of_summary summary in
        if Hashtbl.mem baseline_by_name identity.name then None
        else Some identity.name)
      summaries
    |> List.sort String.compare
  in
  let baseline_only =
    Hashtbl.fold
      (fun name _ names ->
        if Hashtbl.mem current_by_name name then names else name :: names)
      baseline_by_name []
    |> List.sort String.compare
  in
  Format.printf "\nComparison against %d compatible report(s)\n"
    (List.length baselines);
  if List.exists (fun report -> report.legacy) baseline_reports then
    Format.printf
      "Legacy schema-3 reports do not record workload scale. Scenario names, \
       suites, boundaries, payloads, latency, allocation, and available sizes \
       are still validated and compared.\n";
  if current_only <> [] then
    Format.printf "Current-only scenarios: %s\n"
      (String.concat ", " current_only);
  if baseline_only <> [] then
    Format.printf "Baseline-only scenarios: %s\n"
      (String.concat ", " baseline_only);
  if (current_only <> [] || baseline_only <> []) && not allow_scenario_drift
  then
    failwith
      "benchmark scenario sets differ; inspect the reported names and pass \
       --allow-scenario-drift only for an intentional comparison";
  Format.printf "%-43s %-13s %14s %14s %10s\n%!" "scenario" "metric" "baseline"
    "current" "delta";
  List.iter
    (fun summary ->
      let identity, current = comparable_of_summary summary in
      match Hashtbl.find_opt baseline_by_name identity.name with
      | None -> ()
      | Some previous_values ->
          List.iter
            (fun previous ->
              require_scenario_identity identity previous.identity)
            previous_values;
          let metric name project current =
            print_metric identity.name name
              (baseline_median project previous_values)
              current
          in
          metric "latency-ns"
            (fun value -> value.latency_nanoseconds)
            current.latency_nanoseconds;
          metric "minor-B/op"
            (fun value -> value.minor_bytes_per_operation)
            current.minor_bytes_per_operation;
          metric "major-B/op"
            (fun value -> value.major_bytes_per_operation)
            current.major_bytes_per_operation;
          metric "promoted-B/op"
            (fun value -> value.promoted_bytes_per_operation)
            current.promoted_bytes_per_operation;
          metric "retained-B"
            (fun value -> value.retained_bytes)
            current.retained_bytes;
          metric "encoded-B"
            (fun value -> value.encoded_bytes)
            current.encoded_bytes)
    summaries
