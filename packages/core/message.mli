type t =
  | Text of { tag : string; message : string }
  | Untyped of Value.t
  | Open of (Snapshot.fragment, Snapshot.error) result
  | Typed : ('a, 'builder) Schema.t * 'a -> t

type object_
type field
type open_patch

type open_builder = {
  untyped : object_;
  field : 'a. string -> 'a Type.t -> 'a -> field;
  object_ : string -> open_author -> field;
  error :
    'error.
    'error Error.t -> ?backtrace:Printexc.raw_backtrace -> 'error -> open_patch;
  seal : object_ -> open_patch;
}

and open_author = open_builder -> open_patch

type builder = {
  text : 'a. tag:string -> ('a, Format.formatter, unit, t) format4 -> 'a;
  untyped : object_;
  field : 'a. string -> 'a Type.t -> 'a -> field;
  object_ : string -> open_author -> field;
  seal : object_ -> t;
  value : Value.t -> t;
  error :
    'error. 'error Error.t -> ?backtrace:Printexc.raw_backtrace -> 'error -> t;
  typed : 'a 'builder. ('a, 'builder) Schema.t -> 'a -> t;
}

type author = builder -> t

val ( |+ ) : object_ -> field -> object_
val open_builder : open_builder
val open_patch_of_value : Value.t -> open_patch

val open_patch_fragment :
  open_patch -> (Snapshot.fragment, Snapshot.error) result

val open_patch_has_error : open_patch -> bool
val builder : builder
