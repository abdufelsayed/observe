type value
type fragment
type t

type integer =
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Decimal of string

type truncation =
  | Depth
  | Object_fields
  | Collection
  | String_bytes
  | Bytes_length
  | Nodes
  | Total_bytes

type view =
  [ `Null
  | `Bool of bool
  | `Integer of integer
  | `Float of float
  | `String of string
  | `Bytes of string
  | `Truncated of truncation
  | `Truncated_list of t list * truncation
  | `Truncated_object of (string * t) list * truncation
  | `List of t list
  | `Object of (string * t) list
  | `Variant of string * bool * t option ]

type error =
  | Limit_exceeded
  | Invalid_utf8
  | Duplicate_field
  | Unsupported
  | Conversion_failed

type context

val create_context : ?limits:Log_limits.t -> unit -> context
val limits : context -> Log_limits.t
val object_field_limit : context -> int
val collection_length_limit : context -> int
val check_depth : context -> depth:int -> (unit, error) result

val enter : context -> (unit, error) result
(** Charge one productive traversal step. Steps are monotonic across localized
    rollback so unproductive recursive descriptions cannot reset their fuel. *)

type checkpoint

val checkpoint : context -> checkpoint
val rollback : context -> checkpoint -> unit

val localize_apply :
  context ->
  depth:int ->
  (context -> depth:int -> 'a -> (value, error) result) ->
  'a ->
  (value, error) result

val truncated : context -> depth:int -> truncation -> (value, error) result
val truncation : value -> truncation option
val own_text : string -> (string, error) result
val valid_text : string -> bool
val copy_text : context -> depth:int -> string -> (string, error) result
val null : context -> depth:int -> (value, error) result
val bool : context -> depth:int -> bool -> (value, error) result
val integer : context -> depth:int -> string -> (value, error) result
val int : context -> depth:int -> int -> (value, error) result
val int32 : context -> depth:int -> int32 -> (value, error) result
val int64 : context -> depth:int -> int64 -> (value, error) result
val float : context -> depth:int -> float -> (value, error) result
val string : context -> depth:int -> string -> (value, error) result
val bytes : context -> depth:int -> bytes -> (value, error) result
val list : context -> depth:int -> value list -> (value, error) result

val object_ :
  context -> depth:int -> (string * value) list -> (value, error) result

val truncated_object :
  context ->
  depth:int ->
  truncation ->
  (string * value) list ->
  (value, error) result

type readiness = Ready | Stop

module List_builder : sig
  type t

  val create : context -> depth:int -> (t, error) result
  val has_capacity : t -> bool
  val prepare : t -> readiness
  val add : t -> value -> readiness
  val truncate : t -> truncation -> unit
  val finish : t -> (value, error) result
end

module Object_builder : sig
  type t

  val create : context -> depth:int -> (t, error) result
  val has_capacity : t -> bool
  val prepare : t -> string -> (readiness, error) result
  val prepare_owned : t -> string -> (readiness, error) result
  val add : t -> value -> readiness
  val truncate : t -> truncation -> unit
  val finish : t -> (value, error) result
end

val build_object_single :
  context ->
  depth:int ->
  string ->
  (unit -> (value, error) result) ->
  (value, error) result

val variant :
  context ->
  depth:int ->
  polymorphic:bool ->
  string ->
  value option ->
  (value, error) result

val seal : context -> value -> fragment
val fragment : value -> fragment

val import : context -> depth:int -> fragment -> (value, error) result
(** Import an already-owned fragment into a larger materialization while
    charging its complete resources and rebasing its depth. *)

val singleton_object_from_owned :
  ?limits:Log_limits.t -> string -> fragment -> (fragment, error) result

val object_from_owned :
  ?limits:Log_limits.t -> (string * fragment) list -> (fragment, error) result

val complete : fragment -> t
val view : t -> view

val is_object : t -> bool
(** Whether the completed value has an object root. *)

val root_field_count : t -> int
(** The number of fields in an object root, or zero for another root shape. *)

val root_has_field_matching : (string -> bool) -> t -> bool
(** Whether an object root contains a field whose name satisfies the predicate.
    The root is traversed at most once. *)

val fragment_is_object : fragment -> bool
val fragment_root_has_field_matching : (string -> bool) -> fragment -> bool

module Object_accumulator : sig
  type state

  val empty : state
  val merge : ?limits:Log_limits.t -> state -> fragment -> (state, error) result

  val merge_disjoint :
    ?limits:Log_limits.t -> state -> fragment -> (state, error) result
  (** Merge one authored contribution while rejecting repeated root names. *)

  val as_fragment : state -> fragment
  val as_value : state -> value
end

val merge_enrichments :
  limits:Log_limits.t ->
  caller:fragment ->
  ((string -> bool) * fragment) list ->
  (fragment * bool, error) result
(** Merge already-owned structured contributions without rebuilding them. Caller
    fields win recursively. Each contribution carries an authoritative-root
    predicate. The boolean reports an incompatible ordinary enrichment path that
    was omitted. *)

val fit_object_extension :
  ?limits:Log_limits.t ->
  fragment ->
  retained_bytes:int ->
  (fragment, error) result
(** Reserve deterministic space for required completed-log metadata. When an
    object root no longer fits, retain its largest safe source-order prefix and
    mark the root as truncated by total size. *)

val validate_extension :
  ?limits:Log_limits.t ->
  fragment option ->
  nodes:int ->
  string_bytes:int ->
  byte_bytes:int ->
  retained_bytes:int ->
  (unit, error) result

val append_json : Buffer.t -> t -> unit

val append_root_json_fields : Buffer.t -> first:bool -> t -> bool
(** Append the fields of an object root without braces. The result is the
    [first] state to use when appending another field. Raises [Invalid_argument]
    for a non-object root. *)

val append_pretty : Pretty.t -> Pretty.placement -> t -> unit

val append_root_pretty_fields : Pretty.t -> trailing:int -> t -> unit
(** Append an object root as top-level tree fields. [trailing] is the number of
    fields the caller will render afterwards and determines the final branch
    marker. Raises [Invalid_argument] for a non-object root. *)
