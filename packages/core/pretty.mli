type error = Invalid_utf8 | Non_finite_float | Unsupported_value | Malformed

exception Error of error

type style = Formatter_intf.style = Plain | Ansi_16 | Ansi_256 | Truecolor
type severity = Debug | Info | Warn | Error
type t

type placement =
  | Inline
  | Root
  | Field of { last : bool; name : string }
  | Constructor of { last : bool; name : string }
  | Index of { last : bool; index : int }

val create : capacity:int -> style -> t
val contents : t -> string
val header : t -> unix_ns:int64 -> severity:severity -> scope:string -> unit
val space : t -> unit
val newline : t -> unit
val text : t -> string -> unit
val trusted_text : t -> string -> unit
val duration : t -> int64 -> unit

val place : t -> placement -> scalar:bool -> bool
(** Write a placement prefix and return [true] when the value's children were
    moved one indentation level deeper. Call {!finish} after writing them. *)

val finish : t -> bool -> unit
val field : t -> last:bool -> name:string -> scalar:bool -> bool
val index : t -> last:bool -> index:int -> scalar:bool -> bool
val constructor : t -> last:bool -> name:string -> scalar:bool -> bool
val null : t -> unit
val bool : t -> bool -> unit
val int : t -> int -> unit
val int32 : t -> int32 -> unit
val int64 : t -> int64 -> unit
val number : t -> string -> unit
val float : t -> float -> unit
val string : t -> string -> unit
val trusted_string : t -> string -> unit
val empty_record : t -> unit
val empty_list : t -> unit
val variant : t -> polymorphic:bool -> string -> unit
val trusted_variant : t -> polymorphic:bool -> string -> unit
val list_start : t -> unit
val list_separator : t -> unit
val list_end : t -> unit

type rendered = Scalar of (t -> unit) | Node of (t -> placement -> unit)

val render : t -> placement -> rendered -> unit
