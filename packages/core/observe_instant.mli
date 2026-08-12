(** An Observe-owned wall-clock occurrence instant. *)

type t

val of_epoch_nanoseconds : int64 -> t
val to_epoch_nanoseconds : t -> int64
val compare : t -> t -> int
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val t : t Observe_type.t
