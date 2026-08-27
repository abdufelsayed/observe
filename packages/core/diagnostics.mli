(** Finite, non-recursive diagnostics for the portable engine. *)

type kind =
  | Not_initialized
  | No_delivery_target
  | Capture_lookup_raised
  | Operation_lookup_raised
  | Clock_unavailable
  | Clock_raised
  | Identity_unavailable
  | Identity_raised
  | Monotonic_clock_unavailable
  | Monotonic_clock_raised
  | Message_evaluation_raised
  | Canonical_freeze_failed
  | Enricher_raised
  | Enricher_invalid
  | Enricher_conflict
  | Enricher_reserved_field
  | Post_seal_set
  | Post_seal_annotate
  | Post_seal_set_level
  | Post_seal_emit
  | Formatting_failed
  | Formatting_raised
  | Console_rejected
  | Console_raised
  | Drain_rejected
  | Drain_raised
  | Drain_delivery_failed
  | Capture_overflow
  | Capture_closed
  | Runtime_closed
  | Redaction_failed
  | Redaction_conflict
  | Drain_redaction_failed
  | Sampling_discarded
  | Sampling_source_raised
  | Sampling_source_invalid
  | Retention_raised
  | Routing_raised

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
