let nanoseconds_per_day = 86_400_000_000_000L

let floor_div value divisor =
  let quotient = Int64.div value divisor in
  let remainder = Int64.rem value divisor in
  if Int64.compare remainder 0L < 0 then Int64.pred quotient else quotient

let expected nanoseconds =
  let days = floor_div nanoseconds nanoseconds_per_day in
  let subday = Int64.sub nanoseconds (Int64.mul days nanoseconds_per_day) in
  let timestamp = Ptime.v (Int64.to_int days, Int64.mul subday 1_000L) in
  let year, month, day = Ptime.to_date timestamp in
  Format.sprintf "%04d-%02d-%02d.jsonl" year month day

let count () =
  match Sys.getenv_opt "OBSERVE_QCHECK_COUNT" with
  | None -> 1_000
  | Some value -> int_of_string value

let date_matches_ptime =
  QCheck.Test.make ~count:(count ()) ~name:"UTC date agrees with Ptime"
    QCheck.int64 (fun nanoseconds ->
      let timestamp = Observe.Timestamp.of_unix_ns nanoseconds in
      String.equal
        (Observe_fs_date.Date.filename timestamp)
        (expected nanoseconds))

let () =
  Alcotest.run "observe-fs-date-properties"
    [
      ( "pbt:observe-fs:date",
        [ QCheck_alcotest.to_alcotest ~speed_level:`Quick date_matches_ptime ]
      );
    ]
