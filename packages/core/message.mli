type untyped

type t =
  | Text of { tag : string; message : string }
  | Untyped of untyped
  | Typed : ('a, 'builder) Schema.t * 'a -> t

type object_
type field
type untyped_patch

type untyped_builder = {
  untyped : object_;
  field : 'a. string -> 'a Type.t -> 'a -> field;
  object_ : string -> untyped_author -> field;
  error :
    'error.
    using:'error Error.t ->
    ?backtrace:Printexc.raw_backtrace ->
    'error ->
    untyped_patch;
  seal : object_ -> untyped_patch;
}

and untyped_author = untyped_builder -> untyped_patch

type builder = {
  text : 'a. tag:string -> ('a, Format.formatter, unit, t) format4 -> 'a;
  untyped : object_;
  field : 'a. string -> 'a Type.t -> 'a -> field;
  object_ : string -> untyped_author -> field;
  seal : object_ -> t;
  error :
    'error.
    using:'error Error.t -> ?backtrace:Printexc.raw_backtrace -> 'error -> t;
  typed : 'a 'builder. using:('a, 'builder) Schema.t -> 'a -> t;
}

type author = builder -> t

val ( |+ ) : object_ -> field -> object_
val untyped_builder : untyped_builder
val untyped_patch_of_value : Value.t -> untyped_patch
val untyped_message_of_value : Value.t -> t

val materialize_untyped_patch :
  untyped_patch -> (Snapshot.fragment, Snapshot.error) result

val materialize_untyped : untyped -> (Snapshot.fragment, Snapshot.error) result
val untyped_patch_has_error : untyped_patch -> bool
val builder : builder
