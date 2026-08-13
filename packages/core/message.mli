type t =
  | Text of { tag : string; message : string }
  | Untyped of Value.t
  | Typed : 'a Type.t * 'a -> t

type builder = {
  text : 'a. tag:string -> ('a, Format.formatter, unit, t) format4 -> 'a;
  untyped : Value.t -> t;
  typed : 'a. 'a Type.t -> 'a -> t;
}

type author = builder -> t

val builder : builder
