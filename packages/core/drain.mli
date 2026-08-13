(** Additional-output capabilities. *)

type acceptance = Accepted | Rejected
type t

val create : (Log.t -> acceptance) -> t
(** Construct a drain from an immediate ownership callback. Before returning
    [Accepted], a drain must synchronously copy or project every part of the log
    that it will use later. The envelope is read-only, but represented OCaml
    values are retained by reference and are not deep snapshots. [Accepted]
    makes no acknowledgement, ordering, or durability claim. *)

val offer : t -> Log.t -> acceptance
(** Invoke the drain callback. Engine code owns exception containment. *)

module Integration : sig
  val report_failure : unit -> unit
  (** Report that asynchronous work accepted by a drain later failed. This
      increments one bounded, non-recursive process diagnostic. It performs no
      logging, formatting, callback, or I/O. *)
end
