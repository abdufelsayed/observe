(** Process-wide static logging authoring. *)

type message = Message.t
type object_ = Message.object_
type field = Message.field
type untyped_patch = Message.untyped_patch

type untyped_builder = Message.untyped_builder = {
  untyped : object_;
  field : 'a. string -> 'a Type.t -> 'a -> field;
  object_ : string -> (untyped_builder -> untyped_patch) -> field;
  error :
    'error.
    using:'error Error.t ->
    ?backtrace:Printexc.raw_backtrace ->
    'error ->
    untyped_patch;
  seal : object_ -> untyped_patch;
}

type builder = private {
  text : 'a. tag:string -> ('a, Format.formatter, unit, message) format4 -> 'a;
  untyped : object_;
  field : 'a. string -> 'a Type.t -> 'a -> field;
  object_ : string -> (untyped_builder -> untyped_patch) -> field;
  seal : object_ -> message;
  error :
    'error.
    using:'error Error.t ->
    ?backtrace:Printexc.raw_backtrace ->
    'error ->
    message;
  typed : 'a 'builder. using:('a, 'builder) Schema.t -> 'a -> message;
}

type author = builder -> message
type ('builder, 'patch) t

val log :
  ?operation:('operation_builder, 'operation_patch) t ->
  level:Level.t ->
  author ->
  unit
(** Auto-emit one point log at a computed level. The active route and level are
    checked before [author] runs. [operation] explicitly associates the separate
    point observation with that wide-log occurrence. *)

val debug : ?operation:('builder, 'patch) t -> author -> unit
val info : ?operation:('builder, 'patch) t -> author -> unit
val warn : ?operation:('builder, 'patch) t -> author -> unit
val error : ?operation:('builder, 'patch) t -> author -> unit
val ( |+ ) : object_ -> field -> object_

val create :
  ?parent:('parent_builder, 'parent_patch) t ->
  name:string ->
  unit ->
  (untyped_builder, untyped_patch) t
(** Start an empty untyped wide log at [Info]. [name] must be non-empty. When no
    route can accept observations, or a required runtime capability is
    unavailable, the returned handle is inert. *)

val create_typed :
  ?parent:('parent_builder, 'parent_patch) t ->
  name:string ->
  using:('record, 'builder) Schema.t ->
  unit ->
  ('builder, 'record Schema.patch) t
(** Start an empty wide log locked to one declared record schema. Contributions
    are sparse patches; no declared field is required before completion. *)

val set : ('builder, 'patch) t -> ('builder -> 'patch) -> unit
(** Contribute one record-shaped patch. [author] runs only while the handle is
    active. Successive objects merge recursively; later non-object values
    replace earlier values at the same field. A failed contribution seals and
    withholds the lifecycle. *)

val set_level : ('builder, 'patch) t -> level:Level.t -> unit
(** Replace the explicit level. The last explicit level wins over an
    error-derived [Error], regardless of call order. *)

val annotate : ('builder, 'patch) t -> level:Level.t -> (unit -> string) -> unit
(** Append one explicit timestamped entry to the completed wide operation. The
    callback runs only while the handle is active. Annotation levels contribute
    to the operation's derived level; an explicit [set_level] still wins.
    Separate point logs are never copied into this list. *)

val emit : ('builder, 'patch) t -> unit
(** Seal and attempt to publish the wide log exactly once. Admission uses the
    final level. Completion failures remain sealed and are diagnosed. *)

type current_error =
  | Not_bound
  | Expected_open
  | Expected_typed
  | Schema_mismatch

exception Current_error of current_error

val current : unit -> (untyped_builder, untyped_patch) t
(** Return the open wide log bound by the innermost operation scope. Raises
    [Current_error Not_bound] outside a scope and [Current_error Expected_open]
    when the current operation is schema-locked. *)

val current_typed :
  using:('record, 'builder) Schema.t -> ('builder, 'record Schema.patch) t
(** Return the schema-locked wide log bound by the innermost operation scope.
    The supplied schema must be the same schema instance used to start the
    operation. Raises [Current_error Not_bound], [Current_error Expected_typed],
    or [Current_error Schema_mismatch] when that contract is not met. *)

val engine_wide : ('builder, 'patch) t -> Engine.wide

val engine_current : ('builder, 'patch) t -> Engine.current
(** Internal bridge used by runtime compositions. *)

val contribute_error :
  ('builder, 'patch) t ->
  'error Error.t ->
  ?backtrace:Printexc.raw_backtrace ->
  'error ->
  bool
(** Internal schema-independent error contribution used by bounded operations.
    Returns whether the error contribution was accepted. *)
