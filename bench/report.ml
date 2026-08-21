type entry = {
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
  minor_collections_per_operation : float;
  major_collections_per_operation : float;
  r_squared : float option;
  samples : int;
  measured_nanoseconds : int64;
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
  word_size_bits : int;
}
[@@deriving observe]

type report = {
  schema_version : int;
  metadata : metadata;
  results : entry list;
}
[@@deriving observe]

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

let metadata ~commit ~suite (configuration : Measurement.configuration) =
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
    word_size_bits = Sys.word_size;
  }

let entry (measurement : Measurement.t) =
  {
    name = measurement.name;
    suite = measurement.suite;
    boundary = measurement.boundary;
    payload = measurement.payload;
    nanoseconds_per_operation = measurement.nanoseconds_per_operation;
    operations_per_second = measurement.operations_per_second;
    minor_bytes_per_operation = measurement.minor_bytes_per_operation;
    major_bytes_per_operation = measurement.major_bytes_per_operation;
    promoted_bytes_per_operation = measurement.promoted_bytes_per_operation;
    retained_bytes = measurement.retained_bytes;
    minor_collections_per_operation =
      measurement.minor_collections_per_operation;
    major_collections_per_operation =
      measurement.major_collections_per_operation;
    r_squared = measurement.r_squared;
    samples = measurement.samples;
    measured_nanoseconds = measurement.measured_nanoseconds;
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

let write_json ~path metadata measurements =
  let report =
    { schema_version = 2; metadata; results = List.map entry measurements }
  in
  write_file path (Observe.Type.to_json_string report_t report)

let print_table measurements =
  Format.printf "%-43s %12s %12s %12s %12s %12s %8s\n%!" "scenario" "ns/op"
    "ops/s" "minor B/op" "major B/op" "retained B" "samples";
  List.iter
    (fun (measurement : Measurement.t) ->
      Format.printf "%-43s %12.1f %12.0f %12.1f %12.1f %12s %8d\n%!"
        measurement.name measurement.nanoseconds_per_operation
        measurement.operations_per_second measurement.minor_bytes_per_operation
        measurement.major_bytes_per_operation
        (match measurement.retained_bytes with
        | None -> "n/a"
        | Some bytes -> Format.asprintf "%.0f" bytes)
        measurement.samples)
    measurements

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let read_report path =
  match Observe.Type.of_json_string report_t (read_file path) with
  | Ok report -> report
  | Error (`Msg message) -> failwith ("invalid benchmark report: " ^ message)

let percentage_delta ~before ~after =
  if before = 0. then None else Some (((after /. before) -. 1.) *. 100.)

let print_comparison ~baseline measurements =
  let baseline_report = read_report baseline in
  let baseline_by_name = Hashtbl.create (List.length baseline_report.results) in
  List.iter
    (fun entry -> Hashtbl.replace baseline_by_name entry.name entry)
    baseline_report.results;
  Format.printf "\nComparison against %s\n" baseline;
  Format.printf "%-43s %12s %12s %10s\n%!" "scenario" "baseline ns" "current ns"
    "delta";
  List.iter
    (fun (measurement : Measurement.t) ->
      match Hashtbl.find_opt baseline_by_name measurement.name with
      | None ->
          Format.printf "%-43s %12s %12.1f %10s\n%!" measurement.name "new"
            measurement.nanoseconds_per_operation "new"
      | Some previous -> (
          match
            percentage_delta ~before:previous.nanoseconds_per_operation
              ~after:measurement.nanoseconds_per_operation
          with
          | None ->
              Format.printf "%-43s %12.1f %12.1f %10s\n%!" measurement.name
                previous.nanoseconds_per_operation
                measurement.nanoseconds_per_operation "n/a"
          | Some delta ->
              Format.printf "%-43s %12.1f %12.1f %+9.1f%%\n%!" measurement.name
                previous.nanoseconds_per_operation
                measurement.nanoseconds_per_operation delta))
    measurements
