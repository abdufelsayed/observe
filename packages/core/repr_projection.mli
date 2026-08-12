(** Generic pretty projection of an opaque Repr description. *)

type node =
  | Null
  | Bool of bool
  | Number of string
  | String of string
  | List of node list
  | Object of (string * node) list
  | Record of (string * node) list
  | Variant of { name : string; polymorphic : bool; payload : node option }

val of_repr : 'a Repr.t -> 'a -> node
(** Project a value through its Repr machine's JSON encoding. Raises
    [Pretty.Error Unsupported_value] when the machine does not support JSON
    projection; other Repr or callback exceptions propagate unchanged. *)

val plan_node : node -> Pretty.rendered
(** Classify a projected tree once and own its rendering step. *)
