type t = int64

let of_epoch_nanoseconds nanoseconds = nanoseconds
let to_epoch_nanoseconds instant = instant
let compare = Int64.compare
let equal = Int64.equal
let pp formatter instant = Format.fprintf formatter "%Ld" instant
let t = Repr.map Repr.int64 of_epoch_nanoseconds to_epoch_nanoseconds
