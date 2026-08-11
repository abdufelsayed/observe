let nanoseconds_per_day = 86_400_000_000_000L
let picoseconds_per_nanosecond = 1_000L

let epoch_nanoseconds (days, picoseconds) =
  let days = Int64.of_int days in
  if
    Int64.compare days 0L < 0
    || Int64.compare picoseconds 0L < 0
    || Int64.compare days (Int64.div Int64.max_int nanoseconds_per_day) > 0
  then None
  else
    let day_nanoseconds = Int64.mul days nanoseconds_per_day in
    let subday_nanoseconds = Int64.div picoseconds picoseconds_per_nanosecond in
    if
      Int64.compare subday_nanoseconds (Int64.sub Int64.max_int day_nanoseconds)
      > 0
    then None
    else Some (Int64.add day_nanoseconds subday_nanoseconds)

let instant parts =
  match epoch_nanoseconds parts with
  | None -> Error Observe.Platform.Unavailable
  | Some nanoseconds -> Ok (Observe.Instant.of_epoch_nanoseconds nanoseconds)
