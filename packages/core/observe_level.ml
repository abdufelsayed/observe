type t = Debug | Info | Warn | Error

let rank = function Debug -> 0 | Info -> 1 | Warn -> 2 | Error -> 3
let compare left right = Int.compare (rank left) (rank right)
let equal left right = compare left right = 0

let to_string = function
  | Debug -> "debug"
  | Info -> "info"
  | Warn -> "warn"
  | Error -> "error"

let pp formatter level = Format.pp_print_string formatter (to_string level)

let t =
  Observe_type.enum "observe.level"
    [ ("debug", Debug); ("info", Info); ("warn", Warn); ("error", Error) ]
