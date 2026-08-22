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
(** Distinguish an auto-emitted point observation from a completed wide
    operation without inspecting presentation output. *)

val operation : t -> operation option
(** The immutable operation envelope on a wide observation. Point observations
    return [None], including correlated points. *)

val correlation_id : t -> string option
(** The associated wide-log occurrence identifier on a point log. Wide logs
    carry their identity in [operation] and return [None] here. *)

val operation_name : operation -> string
val operation_id : operation -> string
val operation_parent_id : operation -> string option

val operation_duration_ns : operation -> int64
(** The non-negative monotonic elapsed duration. *)

module Producer : sig
  type body =
    | Text of { tag : string; message : string }
    | Structured of { origin : structured_origin; value : Snapshot.fragment }

  val make :
    service:string ->
    ?environment:string ->
    ?version:string ->
    timestamp:Timestamp.t ->
    level:Level.t ->
    ?correlation_id:string ->
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
