type operation = Inspect | Create_directory | Open | Write | Close
type error = { operation : operation; path : string; cause : Unix.error }
type file
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
