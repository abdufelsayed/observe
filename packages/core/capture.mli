(** Deterministic, finite-capacity capture of completed log envelopes. *)

type t

val default_capacity : int

val create : capacity:int -> t
(** [create ~capacity] creates an open capture. [capacity] must be positive. *)

val logs : t -> Log.t list
(** Return immutable completed logs in accepted order. *)

val diagnostics : t -> Diagnostics.entry list
val close : t -> unit

val offer : t -> Log.t -> [ `Accepted | `Overflow | `Closed ]
(** Offer one completed envelope. Open captures retain the earliest [capacity]
    entries. Overflow and closed offers are diagnosed locally. *)

val record : t -> Diagnostics.kind -> unit
(** Record a capture-local engine diagnostic. *)
