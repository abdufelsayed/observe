open Ppxlib

val lident : loc:Location.t -> string -> Longident.t loc
val evar : loc:Location.t -> string -> expression
val estr : loc:Location.t -> string -> expression
val indexed : string -> int -> string
val indexed2 : string -> int -> int -> string
val apply : loc:Location.t -> expression -> expression list -> expression
val call : loc:Location.t -> string -> expression list -> expression

val eapply :
  loc:Location.t -> string -> (arg_label * expression) list -> expression

val lambda : loc:Location.t -> pattern list -> expression -> expression
val tuple_expression : loc:Location.t -> expression list -> expression
val tuple_pattern : loc:Location.t -> pattern list -> pattern
val sequence : loc:Location.t -> expression list -> expression -> expression

val constructor_pattern :
  loc:Location.t -> constructor_declaration -> pattern option -> pattern

val field_binders :
  prefix:string ->
  label_declaration list ->
  (label_declaration * string * pattern) list

val inline_record_pattern :
  loc:Location.t ->
  label_declaration list ->
  ('a * 'b * pattern) list ->
  pattern
