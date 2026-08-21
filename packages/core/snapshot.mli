type t =
  | Null
  | Bool of bool
  | Integer of string
  | Float of float
  | String of string
  | List of t list
  | Object of (string * t) list
  | Variant of { name : string; polymorphic : bool; payload : t option }

type error = Limit_exceeded | Invalid_utf8 | Unsupported | Conversion_failed
type context

val width_limit : int
val create_context : unit -> context
val check_depth : depth:int -> (unit, error) result
val copy_text : context -> depth:int -> string -> (string, error) result
val null : context -> depth:int -> (t, error) result
val bool : context -> depth:int -> bool -> (t, error) result
val integer : context -> depth:int -> string -> (t, error) result
val float : context -> depth:int -> float -> (t, error) result
val string : context -> depth:int -> string -> (t, error) result
val bytes : context -> depth:int -> bytes -> (t, error) result
val list : context -> depth:int -> t list -> (t, error) result
val object_ : context -> depth:int -> (string * t) list -> (t, error) result

val variant :
  context ->
  depth:int ->
  polymorphic:bool ->
  string ->
  t option ->
  (t, error) result

val refreeze : t -> (t, error) result
val refreeze_into : context -> t -> (t, error) result
val merge_object : t -> t -> (t, error) result
val append_json : Buffer.t -> t -> unit
val append_pretty : Pretty.t -> Pretty.placement -> t -> unit
