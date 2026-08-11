(** Unix platform support for Observe.

    Most Lwt applications should initialize logging through [Observe_lwt_unix].
    Integration authors can use {!Platform} with [Observe.Runtime.Make]. *)

module Platform : Observe.Platform.S with type t = unit
(** The Observe platform mechanism backed by the OS wall clock and the process
    standard-error descriptor.

    Terminal writes are synchronous. [Accepted] means that every supplied byte
    was handed to the descriptor. It does not promise atomicity, ordering,
    flushing, or durability. *)
