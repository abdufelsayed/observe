type error = Closed
type t
type identity

val create : unit -> t
val create_identity : unit -> identity

val register :
  t ->
  identity:identity ->
  flush:(unit -> unit Lwt.t) ->
  shutdown:(unit -> unit Lwt.t) ->
  (unit, error) result

val flush : t -> unit Lwt.t
val shutdown : t -> unit Lwt.t
