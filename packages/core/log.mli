(** Completed admitted logs. *)

type t

type payload =
  | Text of { tag : string; message : string }
  | Free of Value.t
  | Structured : 'a Type.t * 'a -> payload

val service : t -> string
val environment : t -> string option
val version : t -> string option
val instant : t -> Instant.t
val level : t -> Level.t
val payload : t -> payload

module Producer : sig
  val make :
    service:string ->
    ?environment:string ->
    ?version:string ->
    instant:Instant.t ->
    level:Level.t ->
    payload ->
    t
end
