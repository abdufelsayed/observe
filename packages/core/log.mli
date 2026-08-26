(** Completed admitted logs. *)

type t
type operation_reference
type operation
type annotation
type redaction
type redaction_effect = Removed | Replaced | Masked | Failed_closed

type redaction_location =
  | Structured_value of string
  | Text_message
  | Annotation_message of int

type kind =
  | Point of { correlation : operation_reference option }
  | Wide of { operation : operation; annotations : annotation list }

type event =
  | Text of { tag : string; message : string }
  | Structured of { origin : structured_origin }

and structured_origin = Open | Declared of string

val service : t -> string
val environment : t -> string option
val version : t -> string option
val timestamp : t -> Timestamp.t
val level : t -> Level.t
val event : t -> event
val fields : t -> Value.frozen

val redactions : t -> redaction list
(** Disclosure transformations applied before publication. This evidence is
    available to capture and inspection but is not added to formatted output. It
    contains no source value. *)

val redaction_effect : redaction -> redaction_effect
val redaction_location : redaction -> redaction_location

val kind : t -> kind
(** Complete point or wide meaning. Impossible combinations such as a point
    carrying wide-operation facts are not representable. *)

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
    | Text of { tag : string; message : string; fields : Snapshot.fragment }
    | Structured of { origin : structured_origin; fields : Snapshot.fragment }

  val make :
    service:string ->
    ?environment:string ->
    ?version:string ->
    timestamp:Timestamp.t ->
    level:Level.t ->
    kind:kind ->
    ?limits:Log_limits.t ->
    ?redactions:redaction list ->
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

  val fields_fragment : t -> Snapshot.fragment
  (** Internal access to the already-bounded field fragment retained by a
      completed log. *)

  val redaction :
    location:redaction_location -> action:redaction_effect -> redaction

  val with_redaction :
    t ->
    limits:Log_limits.t ->
    ?message:string ->
    ?fields:Snapshot.fragment ->
    ?annotations:annotation list ->
    redactions:redaction list ->
    unit ->
    (t, Snapshot.error) result
end
