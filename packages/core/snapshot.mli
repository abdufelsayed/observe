type value
type fragment
type t

type error =
  | Limit_exceeded
  | Invalid_utf8
  | Duplicate_field
  | Unsupported
  | Conversion_failed

type context

val width_limit : int
val max_string_bytes : int
val create_context : unit -> context
val check_depth : depth:int -> (unit, error) result
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

val object_with_owned_names :
  context -> depth:int -> (string * value) list -> (value, error) result

val variant :
  context ->
  depth:int ->
  polymorphic:bool ->
  string ->
  value option ->
  (value, error) result

val seal : context -> value -> fragment
val singleton_object_from_owned : string -> fragment -> (fragment, error) result
val object_from_owned : (string * fragment) list -> (fragment, error) result
val complete : fragment -> t

val is_object : t -> bool
(** Whether the completed value has an object root. *)

val root_field_count : t -> int
(** The number of fields in an object root, or zero for another root shape. *)

val root_has_field_matching : (string -> bool) -> t -> bool
(** Whether an object root contains a field whose name satisfies the predicate.
    The root is traversed at most once. *)

module Object_accumulator : sig
  type state

  val empty : state
  val merge : state -> fragment -> (state, error) result

  val merge_disjoint : state -> fragment -> (state, error) result
  (** Merge one authored contribution while rejecting repeated root names. *)

  val as_fragment : state -> fragment
end

val validate_extension :
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
