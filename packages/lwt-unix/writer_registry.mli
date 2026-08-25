type error = Closed
type t

val create : unit -> t

val register :
  t ->
  flush:(unit -> unit Lwt.t) ->
  shutdown:(unit -> unit Lwt.t) ->
  (unit, error) result

val flush : t -> unit Lwt.t
val shutdown : t -> unit Lwt.t
