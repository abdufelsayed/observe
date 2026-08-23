(** Completed admitted logs. *)

type t
type kind = Point | Wide
type operation_reference
type operation
type annotation

type event =
  | Text of { tag : string; message : string }
  | Structured of { origin : structured_origin; value : Value.frozen }

and structured_origin = Open | Declared of string

val service : t -> string
val environment : t -> string option
val version : t -> string option
val timestamp : t -> Timestamp.t
val level : t -> Level.t
val event : t -> event

val kind : t -> kind
(** Distinguish an auto-emitted point observation from a completed wide
    operation without inspecting presentation output. *)

val operation : t -> operation option
(** The completed wide operation. Point observations return [None], including
    correlated points. *)

val correlation : t -> operation_reference option
(** The current or explicitly associated operation on a separate point log. *)

val annotations : t -> annotation list
(** Explicit timestamped entries accumulated inside a wide operation. Point logs
    have no annotations. *)

val operation_reference_name : operation_reference -> string
val operation_reference_id : operation_reference -> string
val operation_name : operation -> string
val operation_id : operation -> string
val operation_parent : operation -> operation_reference option

val operation_duration_ns : operation -> int64
(** The non-negative monotonic elapsed duration. *)

val annotation_timestamp : annotation -> Timestamp.t
val annotation_level : annotation -> Level.t
val annotation_message : annotation -> string

module Producer : sig
  type event =
    | Text of { tag : string; message : string }
    | Structured of { origin : structured_origin; value : Snapshot.fragment }

  val make :
    service:string ->
    ?environment:string ->
    ?version:string ->
    timestamp:Timestamp.t ->
    level:Level.t ->
    ?correlation:operation_reference ->
    ?operation:operation ->
    ?annotations:annotation list ->
    event ->
    (t, Snapshot.error) result

  val operation_reference : name:string -> id:string -> operation_reference

  val operation :
    name:string ->
    id:string ->
    ?parent:operation_reference ->
    duration_ns:int64 ->
    unit ->
    operation

  val annotation :
    timestamp:Timestamp.t -> level:Level.t -> message:string -> annotation
end
