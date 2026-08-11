(** Private structural projection used only by readable formatting. *)

type t =
  | Null
  | Bool of bool
  | Number of string
  | String of string
  | List of t list
  | Object of (string * t) list

type error = Invalid_utf8 | Non_finite_float | Unsupported_value | Malformed

val of_value : Observe_value.t -> (t, error) result
val of_repr : 'a Repr.t -> 'a -> (t, error) result
