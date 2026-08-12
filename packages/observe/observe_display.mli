(** Private structural projection used only by readable formatting. *)

type t =
  | Null
  | Bool of bool
  | Number of string
  | String of string
  | List of t list
  | Object of (string * t) list
  | Record of (string * t) list
  | Variant of { name : string; polymorphic : bool; payload : t option }

type error = Invalid_utf8 | Non_finite_float | Unsupported_value | Malformed

val of_repr : 'a Repr.t -> 'a -> (t, error) result
val valid_string : string -> (string, error) result
