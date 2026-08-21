(** Process-wide static logging authoring. *)

type message
type object_ = Message.object_
type field = Message.field
type open_patch = Message.open_patch

type open_builder = Message.open_builder = {
  untyped : object_;
  field : 'a. string -> 'a Type.t -> 'a -> field;
  object_ : string -> (open_builder -> open_patch) -> field;
  error :
    'error.
    'error Error.t -> ?backtrace:Printexc.raw_backtrace -> 'error -> open_patch;
  seal : object_ -> open_patch;
}

type builder = private {
  text : 'a. tag:string -> ('a, Format.formatter, unit, message) format4 -> 'a;
  untyped : object_;
  field : 'a. string -> 'a Type.t -> 'a -> field;
  object_ : string -> (open_builder -> open_patch) -> field;
  seal : object_ -> message;
  value : Value.t -> message;
  error :
    'error.
    'error Error.t -> ?backtrace:Printexc.raw_backtrace -> 'error -> message;
  typed : 'a 'builder. ('a, 'builder) Schema.t -> 'a -> message;
}

type author = builder -> message

val log : level:Level.t -> author -> unit
(** Auto-emit one point log at a computed level. The active route and level are
    checked before [author] runs. *)

val debug : author -> unit
val info : author -> unit
val warn : author -> unit
val error : author -> unit
val ( |+ ) : object_ -> field -> object_

type ('builder, 'patch) t

val create : name:string -> unit -> (open_builder, open_patch) t
(** Start an empty open wide log at [Info]. [name] must be non-empty. When no
    route can accept observations, or a required runtime capability is
    unavailable, the returned handle is inert. *)

val create_typed :
  name:string ->
  ('record, 'builder) Schema.t ->
  ('builder, 'record Schema.patch) t
(** Start an empty wide log locked to one declared record schema. Contributions
    are sparse patches; no declared field is required before completion. *)

val set : ('builder, 'patch) t -> ('builder -> 'patch) -> unit
(** Contribute one record-shaped patch. [author] runs only while the handle is
    active. Successive objects merge recursively; later non-object values
    replace earlier values at the same field. A failed contribution seals and
    withholds the lifecycle. *)

val set_level : ('builder, 'patch) t -> Level.t -> unit
(** Replace the explicit level. The last explicit level wins over an
    error-derived [Error], regardless of call order. *)

val emit : ('builder, 'patch) t -> unit
(** Seal and attempt to publish the wide log exactly once. Admission uses the
    final level. Completion failures remain sealed and are diagnosed. *)
