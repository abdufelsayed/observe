(** Composition and publication of an Observe I/O implementation. *)

type init_error = Already_initialized | IO_already_registered
type capture_error = IO_already_registered | Invalid_capacity of int

exception Init_error of init_error

module Make (IO : Io.S) : sig
  type +'a io = 'a IO.t
  type t

  val create : IO.state -> t
  val init : t -> Config.t -> (unit, init_error) result
  val init_exn : t -> Config.t -> unit

  val with_capture :
    t ->
    Config.t ->
    ?capacity:int ->
    (Capture.t -> 'a io) ->
    ('a, capture_error) result io

  val with_wide : t -> Engine.wide -> (unit -> 'a io) -> 'a io
end

val emit_point :
  ?correlation_id:string -> level:Level.t -> Message.author -> unit
(** Internal entry point for the static [Logs] API. *)

val create_wide :
  ?parent:Engine.wide ->
  name:string ->
  origin:Log.structured_origin ->
  unit ->
  Engine.wide
