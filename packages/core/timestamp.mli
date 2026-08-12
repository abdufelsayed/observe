(** An Observe-owned Unix timestamp with nanosecond precision. *)

type t

val of_unix_ns : int64 -> t
val to_unix_ns : t -> int64
val compare : t -> t -> int
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val t : t Type.t
