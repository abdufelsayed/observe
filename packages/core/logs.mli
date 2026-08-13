(** Process-wide static logging authoring. *)

type message

type builder = private {
  text : 'a. tag:string -> ('a, Format.formatter, unit, message) format4 -> 'a;
  untyped : Value.t -> message;
  typed : 'a. 'a Type.t -> 'a -> message;
}

type author = builder -> message

val emit : level:Level.t -> author -> unit
val debug : author -> unit
val info : author -> unit
val warn : author -> unit
val error : author -> unit
