(** Completed admitted logs. *)

type t

type body =
  | Text of { tag : string; message : string }
  | Untyped of Value.t
  | Typed : 'a Type.t * 'a -> body

val service : t -> string
val environment : t -> string option
val version : t -> string option
val timestamp : t -> Timestamp.t
val level : t -> Level.t
val body : t -> body

module Producer : sig
  val make :
    service:string ->
    ?environment:string ->
    ?version:string ->
    timestamp:Timestamp.t ->
    level:Level.t ->
    body ->
    t
end
