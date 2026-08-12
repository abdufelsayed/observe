(** Logging severity in admission order. *)

type t = Debug | Info | Warn | Error

val compare : t -> t -> int
val equal : t -> t -> bool
val to_string : t -> string
val pp : Format.formatter -> t -> unit
val t : t Type.t
