(** Completion-aware retention over one disclosure-safe completed log. *)

type t

val create : keep:(Log.t -> bool) -> t
(** Construct a policy that can rescue a completed log from base sampling.
    Returning [false] defers to the base sampling decision; it does not force a
    drop. The callback runs synchronously and may be invoked concurrently for
    different observations, so shared state must be concurrency-safe. Ordinary
    exceptions are contained by the engine; runtime control exceptions remain
    native. *)

val keep : t -> Log.t -> bool
(** Private engine invocation. Engine code owns exception containment. *)
