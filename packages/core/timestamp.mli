(** An Observe-owned Unix timestamp with nanosecond precision. *)

type t

val of_unix_ns : int64 -> t
val to_unix_ns : t -> int64

val to_rfc3339 : t -> string
(** Render the exact timestamp in UTC with nine fractional digits. *)

val append_rfc3339 : Buffer.t -> t -> unit
(** Append the same exact UTC representation without allocating an intermediate
    string. *)

val compare : t -> t -> int
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val t : t Type.t
