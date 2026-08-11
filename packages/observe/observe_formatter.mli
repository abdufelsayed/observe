(** Pure projections of completed represented logs. *)

type error = Invalid_utf8 | Non_finite_float | Unsupported_value | Failed
type t

val create : (Observe_log.t -> (string, error) result) -> t
(** Construct a formatter. The callback must perform no I/O. *)

val format : t -> Observe_log.t -> (string, error) result
(** Invoke a formatter without catching callback exceptions. The engine owns
    exception containment so it can preserve its narrow exception policy. *)

val readable : t
val json : t
val json_lines : t
