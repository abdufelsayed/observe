(** Lwt execution for the portable Observe filesystem writer. *)

module IO : sig
  (** Filesystem I/O and scheduler-crossing mechanisms supplied to the Lwt
      maker. Implementations may be Unix-free, including Mirage filesystems. *)

  module type S = sig
    type file
    type error
    type lock
    type notifier

    val create_lock : unit -> lock

    val with_lock : lock -> (unit -> 'a) -> 'a
    (** Protect one short synchronous state transition. The callback performs no
        Lwt effect and the lock must be released if it raises. *)

    val create_notifier : unit -> notifier

    val await : notifier -> unit Lwt.t
    (** Create a waiter for the next notification. The maker protects the
        returned promise from caller cancellation. *)

    val notify : notifier -> unit
    val dispose : notifier -> unit
    val child : string -> string -> string
    val ensure_directory : string -> (unit, error) result Lwt.t
    val open_append : string -> (file, error) result Lwt.t

    val write :
      file -> string -> offset:int -> length:int -> (int, error) result Lwt.t
    (** Return the number of bytes written from the requested slice. Partial
        progress is supported; zero progress is a writer failure. The
        implementation must not retain the string after the promise settles. *)

    val flush : file -> (unit, error) result Lwt.t
    val close : file -> (unit, error) result Lwt.t
    val pp_error : Format.formatter -> error -> unit
  end
end

module Make (IO : IO.S) : sig
  type error =
    | Invalid_path
    | Invalid_capacity of int
    | Io of IO.error
    | Zero_progress
    | Invalid_write_count of int
    | Unexpected of exn

  type t

  val create : path:string -> ?capacity:int -> unit -> (t, error) result Lwt.t
  val drain : t -> Observe.Drain.t
  val flush : t -> (unit, error) result Lwt.t
  val shutdown : t -> (unit, error) result Lwt.t
  val pp_error : Format.formatter -> error -> unit
end
