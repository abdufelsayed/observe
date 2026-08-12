open Bechamel
open Toolkit

type configuration = { quota_ms : int; limit : int; allocation_runs : int }

type t = {
  name : string;
  suite : string;
  boundary : string;
  payload : string;
  nanoseconds_per_operation : float;
  operations_per_second : float;
  minor_bytes_per_operation : float;
  major_bytes_per_operation : float;
  promoted_bytes_per_operation : float;
  minor_collections_per_operation : float;
  major_collections_per_operation : float;
  r_squared : float option;
  samples : int;
  measured_nanoseconds : int64;
}

let analysis =
  Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:[| Measure.run |]

let analyze measure raw =
  let result = Analyze.one analysis measure raw in
  match Analyze.OLS.estimates result with
  | Some [ estimate ] when Float.is_finite estimate ->
      ( estimate,
        Option.bind (Analyze.OLS.r_square result) (fun value ->
            if Float.is_finite value then Some value else None) )
  | Some _ | None ->
      failwith ("Bechamel could not analyze " ^ Measure.label measure)

let allocations runs operation =
  for _ = 1 to min 1_000 runs do
    operation ()
  done;
  Gc.full_major ();
  let minor_before, promoted_before, major_before = Gc.counters () in
  let before = Gc.quick_stat () in
  for _ = 1 to runs do
    operation ()
  done;
  let minor_after, promoted_after, major_after = Gc.counters () in
  let after = Gc.quick_stat () in
  let divisor = float_of_int runs in
  let bytes_per_word = float_of_int (Sys.word_size / 8) in
  ( (minor_after -. minor_before) *. bytes_per_word /. divisor,
    (major_after -. major_before) *. bytes_per_word /. divisor,
    (promoted_after -. promoted_before) *. bytes_per_word /. divisor,
    float_of_int (after.minor_collections - before.minor_collections) /. divisor,
    float_of_int (after.major_collections - before.major_collections) /. divisor
  )

let run configuration scenario operation =
  let test =
    Test.make ~name:(Scenario.name scenario) (Staged.stage operation)
  in
  let element =
    match Test.elements test with
    | [ element ] -> element
    | _ -> failwith "a benchmark scenario expanded to more than one test"
  in
  let measures = Instance.[ monotonic_clock ] in
  let benchmark_configuration =
    Benchmark.cfg ~limit:configuration.limit ~stabilize:true
      ~quota:(Time.millisecond (float_of_int configuration.quota_ms))
      ()
  in
  let raw = Benchmark.run benchmark_configuration measures element in
  let nanoseconds_per_operation, r_squared =
    analyze Instance.monotonic_clock raw
  in
  if nanoseconds_per_operation <= 0. then
    failwith "Bechamel produced a non-positive operation latency";
  let ( minor_bytes_per_operation,
        major_bytes_per_operation,
        promoted_bytes_per_operation,
        minor_collections_per_operation,
        major_collections_per_operation ) =
    allocations configuration.allocation_runs operation
  in
  {
    name = Scenario.name scenario;
    suite = Scenario.suite_name (Scenario.suite scenario);
    boundary = Scenario.boundary scenario;
    payload = Scenario.payload scenario;
    nanoseconds_per_operation;
    operations_per_second = 1_000_000_000. /. nanoseconds_per_operation;
    minor_bytes_per_operation;
    major_bytes_per_operation;
    promoted_bytes_per_operation;
    minor_collections_per_operation;
    major_collections_per_operation;
    r_squared;
    samples = raw.stats.samples;
    measured_nanoseconds = Time.span_to_uint64_ns raw.stats.time;
  }
