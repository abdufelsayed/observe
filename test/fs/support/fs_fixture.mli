module IO : sig
  type file

  type error =
    | Open_failed
    | Write_failed
    | Flush_failed
    | Close_failed
    | Closed_file

  type lock
  type notifier

  val create_lock : unit -> lock
  val with_lock : lock -> (unit -> 'a) -> 'a
  val create_notifier : unit -> notifier
  val await : notifier -> unit Lwt.t
  val notify : notifier -> unit
  val dispose : notifier -> unit
  val child : dir:string -> name:string -> string
  val ensure_directory : string -> (unit, error) result Lwt.t
  val open_append : string -> (file, error) result Lwt.t

  val write :
    file -> string -> offset:int -> length:int -> (int, error) result Lwt.t

  val flush : file -> (unit, error) result Lwt.t
  val close : file -> (unit, error) result Lwt.t
  val pp_error : Format.formatter -> error -> unit
end

val reset : unit -> unit
val seed : string -> string -> unit
val contents : string -> string
val paths : unit -> string list
val set_max_write : int -> unit
val fail_next_open : unit -> unit
val fail_next_write : unit -> unit
val fail_next_flush : unit -> unit
val fail_next_close : unit -> unit
val block_writes : unit -> unit -> unit
val flush_count : unit -> int
val close_count : unit -> int
val operations : unit -> [ `Write | `Flush | `Close ] list
val worker_notification_count : unit -> int
