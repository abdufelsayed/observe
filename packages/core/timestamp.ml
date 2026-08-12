type t = int64

let of_unix_ns nanoseconds = nanoseconds
let to_unix_ns timestamp = timestamp
let compare = Int64.compare
let equal = Int64.equal

let pp formatter timestamp =
  Format.pp_print_string formatter (Int64.to_string timestamp)

let t = Type.map Type.int64 of_unix_ns to_unix_ns
