(** Additional-output capabilities. *)

type acceptance = Accepted | Rejected
type t

val create : (Log.t -> acceptance) -> t
(** Construct a drain from an immediate callback over one immutable completed
    observation. A drain can retain [Log.t] safely; it must still own any
    destination-specific projection or mutable delivery state. [Accepted] makes
    no acknowledgement, ordering, or durability claim. The callback runs
    synchronously, may run concurrently for different observations, and must be
    concurrency-safe and promptly return. *)

val with_redaction : redaction:Log_redaction.t -> t -> t
(** Strengthen this destination with an additional disclosure policy. Nested
    wrappers are normalized into one order-independent policy. The destination
    receives only the already globally safe observation and cannot recover
    source data removed earlier. Invalid policy composition raises
    [Log_redaction.Invalid_redaction]. *)

val with_route : when_:(Log.t -> bool) -> t -> t
(** Select this drain from the globally safe completed observation. Nested
    routes compose by conjunction. Ordinary callback exceptions are contained by
    the engine and fail closed for this drain only. Route callbacks run
    synchronously, may run concurrently for different observations, and must
    promptly return. *)

val redaction : t -> Log_redaction.t
(** Private engine access to the normalized destination policy. *)

val has_route : t -> bool
(** Private engine fast-path test. *)

val routed : t -> Log.t -> bool
(** Private engine access to the configured route. *)

val offer : t -> Log.t -> acceptance
(** Invoke the drain callback. Engine code owns exception containment. *)

module Integration : sig
  val report_failure : t -> unit
  (** Report that asynchronous work accepted by this drain later failed.
      Repeated reports for the same drain count once. Reporting increments one
      bounded, non-recursive process diagnostic and performs no logging,
      formatting, callback, or I/O. *)
end
