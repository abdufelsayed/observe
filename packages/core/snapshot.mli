type value
type fragment
type t
type error = Limit_exceeded | Invalid_utf8 | Unsupported | Conversion_failed
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

module Object_accumulator : sig
  type state

  val empty : state
  val merge : state -> fragment -> (state, error) result
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
val append_pretty : Pretty.t -> Pretty.placement -> t -> unit
