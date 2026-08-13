let floor_div value divisor =
  let quotient = Int64.div value divisor in
  let remainder = Int64.rem value divisor in
  if Int64.compare remainder 0L < 0 then Int64.pred quotient else quotient

let digit bytes offset value =
  Bytes.unsafe_set bytes offset (Char.unsafe_chr (Char.code '0' + value))

let filename timestamp =
  let nanoseconds_per_day = 86_400_000_000_000L in
  let days =
    floor_div (Observe.Timestamp.to_unix_ns timestamp) nanoseconds_per_day
  in
  let shifted = Int64.add days 719_468L in
  let era =
    if Int64.compare shifted 0L >= 0 then Int64.div shifted 146_097L
    else Int64.div (Int64.sub shifted 146_096L) 146_097L
  in
  let day_of_era = Int64.sub shifted (Int64.mul era 146_097L) in
  let year_of_era =
    Int64.div
      (Int64.sub
         (Int64.add day_of_era (Int64.div day_of_era 36_524L))
         (Int64.add
            (Int64.div day_of_era 1_460L)
            (Int64.div day_of_era 146_096L)))
      365L
  in
  let year = Int64.add year_of_era (Int64.mul era 400L) in
  let day_of_year =
    Int64.sub day_of_era
      (Int64.add
         (Int64.mul 365L year_of_era)
         (Int64.sub (Int64.div year_of_era 4L) (Int64.div year_of_era 100L)))
  in
  let month_part = Int64.div (Int64.add (Int64.mul 5L day_of_year) 2L) 153L in
  let day =
    Int64.add
      (Int64.sub day_of_year
         (Int64.div (Int64.add (Int64.mul 153L month_part) 2L) 5L))
      1L
  in
  let month =
    Int64.add month_part (if Int64.compare month_part 10L < 0 then 3L else -9L)
  in
  let year = Int64.add year (if Int64.compare month 2L <= 0 then 1L else 0L) in
  let year = Int64.to_int year in
  let month = Int64.to_int month in
  let day = Int64.to_int day in
  let bytes = Bytes.of_string "0000-00-00.jsonl" in
  digit bytes 0 (year / 1_000 mod 10);
  digit bytes 1 (year / 100 mod 10);
  digit bytes 2 (year / 10 mod 10);
  digit bytes 3 (year mod 10);
  digit bytes 5 (month / 10);
  digit bytes 6 (month mod 10);
  digit bytes 8 (day / 10);
  digit bytes 9 (day mod 10);
  Bytes.unsafe_to_string bytes
