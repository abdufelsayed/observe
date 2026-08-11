(** Immutable free-form structured values. *)

type t

val null : t
val bool : bool -> t
val int : int -> t
val float : float -> t
val string : string -> t
val option : t option -> t
val list : t list -> t
val object_ : (string * t) list -> t

val embed : 'a Repr.t -> 'a -> t
(** Retain a typed value and its description without projecting it. *)

val pp : Format.formatter -> t -> unit
val to_string : t -> string

type json_error = Invalid_utf8 | Non_finite_float | Unsupported_value

val is_valid_utf8 : string -> bool

val to_json_string : t -> (string, json_error) result
(** Project one JSON value without repairing invalid strings or non-finite
    primitive floats. Embedded values retain the JSON semantics of their
    supplied representation; unsupported Repr projections return
    [Unsupported_value]. *)
