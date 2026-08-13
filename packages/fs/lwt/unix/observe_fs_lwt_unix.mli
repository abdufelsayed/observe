(** Ready daily NDJSON files for Observe applications using Lwt-Unix. *)

type operation = Inspect | Create_directory | Open | Write | Close

type error =
  | Invalid_directory
  | Invalid_capacity of int
  | Filesystem of { operation : operation; path : string; cause : Unix.error }
  | Zero_progress
  | Invalid_write_count of int
  | Unexpected of exn
  | Lifecycle_closed

exception Error of error
(** Raised by {!create_exn}. *)

val create :
  dir:string -> ?capacity:int -> unit -> (Observe.Drain.t, error) result Lwt.t
(** Prepare [dir], start one bounded worker, join the ready lifecycle, and
    return the configured drain. *)

val create_exn : dir:string -> ?capacity:int -> unit -> Observe.Drain.t Lwt.t
(** Like {!create}, but raise {!Error} with the same typed failure. *)

val pp_error : Format.formatter -> error -> unit
