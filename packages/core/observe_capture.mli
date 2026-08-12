(** Deterministic, finite-capacity capture of completed log envelopes. *)

type t

val default_capacity : int

val create : capacity:int -> t
(** [create ~capacity] creates an open capture. [capacity] must be positive. *)

val logs : t -> Observe_log.t list
(** Return retained entries in accepted order. Typed payload values are retained
    by reference, not deeply copied. *)

val diagnostics : t -> Observe_diagnostics.entry list
val close : t -> unit

val offer : t -> Observe_log.t -> [ `Accepted | `Overflow | `Closed ]
(** Offer one completed envelope. Open captures retain the earliest [capacity]
    entries. Overflow and closed offers are diagnosed locally. *)

val record : t -> Observe_diagnostics.kind -> unit
(** Record a capture-local engine diagnostic. *)
