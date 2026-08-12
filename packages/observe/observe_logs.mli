(** Process-wide static logging authoring. *)

type message

val text : tag:string -> string -> message
val text_lazy : tag:string -> (unit -> string) -> message
val free : (unit -> Observe_value.t) -> message
val structured : 'a Observe_type.t -> 'a -> message
val emit : level:Observe_level.t -> message -> unit
val debug : message -> unit
val info : message -> unit
val warn : message -> unit
val error : message -> unit
