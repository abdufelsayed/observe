(** Finite, non-recursive diagnostics for the portable engine. *)

type kind =
  | Not_initialized
  | No_output
  | Scope_raised
  | Clock_unavailable
  | Clock_raised
  | Authoring_raised
  | Formatting_failed
  | Formatting_raised
  | Console_rejected
  | Console_raised
  | Drain_rejected
  | Drain_raised
  | Capture_overflow
  | Capture_closed

type entry = { kind : kind; count : int }
type store

val create_store : unit -> store
(** [create_store ()] returns an empty bounded diagnostic store. *)

val record_into : store -> kind -> unit
(** [record_into store kind] increments one saturating counter. It invokes no
    consumer callback and performs no logging. *)

val snapshot_store : store -> entry list
(** [snapshot_store store] returns non-zero counters in stable kind order. *)

val record : kind -> unit
(** Record into the process-wide diagnostic store. *)

val snapshot : unit -> entry list
(** Return a non-clearing snapshot of the process-wide diagnostic store. *)
