(** Completed admitted logs. *)

type t

type payload =
  | Text of { tag : string; message : string }
  | Free of Observe_value.t
  | Structured : 'a Repr.t * 'a -> payload

val service : t -> string
val environment : t -> string option
val version : t -> string option
val instant : t -> Observe_instant.t
val level : t -> Observe_level.t
val payload : t -> payload

module Producer : sig
  val make :
    service:string ->
    ?environment:string ->
    ?version:string ->
    instant:Observe_instant.t ->
    level:Observe_level.t ->
    payload ->
    t
end
