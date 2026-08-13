type error = Closed
type t

val create : flush:(unit -> unit Lwt.t) -> shutdown:(unit -> unit Lwt.t) -> t

val register :
  t ->
  flush:(unit -> unit Lwt.t) ->
  shutdown:(unit -> unit Lwt.t) ->
  (unit, error) result

val flush : t -> unit Lwt.t
val shutdown : t -> unit Lwt.t
