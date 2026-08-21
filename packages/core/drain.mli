(** Additional-output capabilities. *)

type acceptance = Accepted | Rejected
type t

val create : (Log.t -> acceptance) -> t
(** Construct a drain from an immediate callback over one immutable completed
    observation. A drain can retain [Log.t] safely; it must still own any
    destination-specific projection or mutable delivery state. [Accepted] makes
    no acknowledgement, ordering, or durability claim. *)

val offer : t -> Log.t -> acceptance
(** Invoke the drain callback. Engine code owns exception containment. *)

module Integration : sig
  val report_failure : unit -> unit
  (** Report that asynchronous work accepted by a drain later failed. This
      increments one bounded, non-recursive process diagnostic. It performs no
      logging, formatting, callback, or I/O. *)
end
