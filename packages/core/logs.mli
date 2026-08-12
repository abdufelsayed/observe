(** Process-wide static logging authoring. *)

type message

val text : tag:string -> string -> message
val text_lazy : tag:string -> (unit -> string) -> message
val free : (unit -> Value.t) -> message
val structured : 'a Type.t -> 'a -> message
val emit : level:Level.t -> message -> unit
val debug : message -> unit
val info : message -> unit
val warn : message -> unit
val error : message -> unit
