exception Invalid_utf8

val null : Buffer.t -> unit
val unit : Buffer.t -> unit -> unit
val bool : Buffer.t -> bool -> unit
val char : Buffer.t -> char -> unit
val decimal_int : Buffer.t -> int -> unit
val decimal_int32 : Buffer.t -> int32 -> unit
val decimal_int64 : Buffer.t -> int64 -> unit
val int : Buffer.t -> int -> unit
val int32 : Buffer.t -> int32 -> unit
val int64 : Buffer.t -> int64 -> unit
val float_to_string : float -> string
val float : Buffer.t -> float -> unit
val string : Buffer.t -> string -> unit
val bytes : Buffer.t -> bytes -> unit
val name : Buffer.t -> string -> unit
val array : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a array -> unit
val list : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a list -> unit
val option : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a option -> unit
