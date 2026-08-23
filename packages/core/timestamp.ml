type t = int64

let of_unix_ns nanoseconds = nanoseconds
let to_unix_ns timestamp = timestamp
let compare = Int64.compare
let equal = Int64.equal
let nanoseconds_per_day = 86_400_000_000_000L

let split_day timestamp =
  let days = Int64.div timestamp nanoseconds_per_day in
  let remainder = Int64.rem timestamp nanoseconds_per_day in
  if Int64.compare remainder 0L < 0 then
    (Int64.to_int (Int64.pred days), Int64.add remainder nanoseconds_per_day)
  else (Int64.to_int days, remainder)

let add_two_digits buffer value =
  Buffer.add_char buffer (Char.unsafe_chr (48 + (value / 10)));
  Buffer.add_char buffer (Char.unsafe_chr (48 + (value mod 10)))

let add_four_digits buffer value =
  add_two_digits buffer (value / 100);
  add_two_digits buffer (value mod 100)

(* Convert a Unix day number to a proleptic Gregorian date. The timestamp's
   int64 nanosecond range is only 1677--2262, so every intermediate fits in an
   OCaml integer on every supported 64-bit platform. This is the civil-from-days
   decomposition with the Unix epoch offset folded in. *)
let append_date buffer days =
  let shifted = days + 719_468 in
  let era = shifted / 146_097 in
  let day_of_era = shifted - (era * 146_097) in
  let year_of_era =
    (day_of_era
    - (day_of_era / 1_460)
    + (day_of_era / 36_524)
    - (day_of_era / 146_096))
    / 365
  in
  let year = year_of_era + (era * 400) in
  let day_of_year =
    day_of_era - ((365 * year_of_era) + (year_of_era / 4) - (year_of_era / 100))
  in
  let month_prime = ((5 * day_of_year) + 2) / 153 in
  let day = day_of_year - (((153 * month_prime) + 2) / 5) + 1 in
  let month = month_prime + if month_prime < 10 then 3 else -9 in
  let year = year + if month <= 2 then 1 else 0 in
  add_four_digits buffer year;
  Buffer.add_char buffer '-';
  add_two_digits buffer month;
  Buffer.add_char buffer '-';
  add_two_digits buffer day

let add_nine_digits buffer value =
  let divisor = ref 100_000_000 in
  while !divisor > 0 do
    Buffer.add_char buffer (Char.unsafe_chr (48 + (value / !divisor mod 10)));
    divisor := !divisor / 10
  done

let append_rfc3339 buffer timestamp =
  let days, remainder = split_day timestamp in
  let seconds = Int64.to_int (Int64.div remainder 1_000_000_000L) in
  let nanoseconds = Int64.to_int (Int64.rem remainder 1_000_000_000L) in
  let hour = seconds / 3_600 in
  let minute = seconds / 60 mod 60 in
  let second = seconds mod 60 in
  append_date buffer days;
  Buffer.add_char buffer 'T';
  add_two_digits buffer hour;
  Buffer.add_char buffer ':';
  add_two_digits buffer minute;
  Buffer.add_char buffer ':';
  add_two_digits buffer second;
  Buffer.add_char buffer '.';
  add_nine_digits buffer nanoseconds;
  Buffer.add_char buffer 'Z'

let to_rfc3339 timestamp =
  let buffer = Buffer.create 30 in
  append_rfc3339 buffer timestamp;
  Buffer.contents buffer

let pp formatter timestamp =
  Format.pp_print_string formatter (Int64.to_string timestamp)

let t = Type.map Type.int64 of_unix_ns to_unix_ns
