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
  val create_lock : unit -> lock
  val with_lock : lock -> (unit -> 'a) -> 'a
  val create_notifier : unit -> notifier
  val await : notifier -> unit t
  val notify : notifier -> unit
  val dispose : notifier -> unit
  val child : string -> string -> string
  val ensure_directory : string -> (unit, error) result t
  val open_append : string -> (file, error) result t

  val write :
    file -> string -> offset:int -> length:int -> (int, error) result t

  val flush : file -> (unit, error) result t
  val close : file -> (unit, error) result t
  val pp_error : Format.formatter -> error -> unit
end
