(** Portable daily filesystem writer for Observe. *)

module IO : sig
  (** Completed mechanisms consumed by the portable writer state machine. *)

  module type S = sig
    type 'a t
    type file
    type error
    type lock
    type notifier

    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

    val async : (unit -> unit t) -> unit
    (** Start one owned worker. Its failure must remain observable by [catch]
        and it must remain alive independently of the constructing callback. *)

    val create_lock : unit -> lock

    val with_lock : lock -> (unit -> 'a) -> 'a
    (** Execute a short synchronous critical section and release the lock after
        normal return or exception. The callback performs no effects. *)

    val create_notifier : unit -> notifier
    val await : notifier -> unit t
    val notify : notifier -> unit

    val dispose : notifier -> unit
    (** A thread-safe scheduler wakeup. [notify] resolves current waiters and
        may coalesce repeated notifications. [dispose] resolves current and
        future waits and releases runtime resources. *)

    val child : string -> string -> string
    val ensure_directory : string -> (unit, error) result t
    val open_append : string -> (file, error) result t

    val write :
      file -> string -> offset:int -> length:int -> (int, error) result t
    (** Write from the requested byte slice. Success returns a count in
        [0..length]; the portable state machine resumes partial writes and
        rejects zero progress. The implementation must not retain the string
        after the returned effect settles; delivery may reuse its private
        backing storage for a later write. *)

    val flush : file -> (unit, error) result t
    (** Complete runtime buffering only. This need not provide [fsync] or crash
        durability. *)

    val close : file -> (unit, error) result t
    val pp_error : Format.formatter -> error -> unit
  end
end

module Make (Implementation : IO.S) : sig
  type error =
    | Invalid_path
    | Invalid_capacity of int
    | Io of Implementation.error
    | Zero_progress
    | Invalid_write_count of int
    | Unexpected of exn

  type t

  val create :
    path:string -> ?capacity:int -> unit -> (t, error) result Implementation.t
  (** Prepare the directory and start one bounded background writer. *)

  val drain : t -> Observe.Drain.t
  (** The synchronous ownership-transfer boundary for application config. *)

  val flush : t -> (unit, error) result Implementation.t
  (** Flush every record accepted before the call. *)

  val shutdown : t -> (unit, error) result Implementation.t
  (** Stop acceptance, drain accepted records, flush, close, and stop. *)

  val pp_error : Format.formatter -> error -> unit
end
