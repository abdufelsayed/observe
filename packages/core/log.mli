(** Completed admitted logs. *)

type t
type kind = Point | Wide
type operation

type body =
  | Text of { tag : string; message : string }
  | Structured of { origin : structured_origin; value : Value.frozen }

and structured_origin = Open | Declared of string

val service : t -> string
val environment : t -> string option
val version : t -> string option
val timestamp : t -> Timestamp.t
val level : t -> Level.t
val body : t -> body
val kind : t -> kind
val operation : t -> operation option
val operation_name : operation -> string
val operation_id : operation -> string
val operation_parent_id : operation -> string option
val operation_duration_ns : operation -> int64

module Producer : sig
  val make :
    service:string ->
    ?environment:string ->
    ?version:string ->
    timestamp:Timestamp.t ->
    level:Level.t ->
    ?operation:operation ->
    body ->
    (t, Snapshot.error) result

  val operation :
    name:string ->
    id:string ->
    ?parent_id:string ->
    duration_ns:int64 ->
    unit ->
    operation
end
