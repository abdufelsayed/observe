(** A completed authoring result. [Engine] invokes an [author] only after
    admission, then seals the returned message with runtime metadata. *)

type t =
  | Text of { tag : string; message : string }
  | Value of Value.t
  | Untyped of (Snapshot.fragment, Snapshot.error) result
  | Typed : ('a, 'builder) Schema.t * 'a -> t

type fragment = (Snapshot.fragment, Snapshot.error) result
type object_ = { fields : (string * fragment) list; has_error : bool }
type field = { name : string; fragment : fragment; has_error : bool }
type untyped_patch = { fragment : fragment; has_error : bool }

type untyped_builder = {
  untyped : object_;
  field : 'a. string -> 'a Type.t -> 'a -> field;
  object_ : string -> untyped_author -> field;
  error :
    'error.
    'error Error.t ->
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
  value : Value.t -> t;
  error :
    'error. 'error Error.t -> ?backtrace:Printexc.raw_backtrace -> 'error -> t;
  typed : 'a 'builder. ('a, 'builder) Schema.t -> 'a -> t;
}

type author = builder -> t

let ( |+ ) object_ field =
  {
    fields = (field.name, field.fragment) :: object_.fields;
    has_error = object_.has_error || field.has_error;
  }

let rec untyped_builder =
  {
    untyped = { fields = []; has_error = false };
    field =
      (fun name description value ->
        { name; fragment = Type.freeze description value; has_error = false });
    object_ =
      (fun name author ->
        let patch = author untyped_builder in
        { name; fragment = patch.fragment; has_error = patch.has_error });
    error =
      (fun interpretation ?backtrace error ->
        {
          fragment = Error.freeze interpretation ?backtrace error;
          has_error = true;
        });
    seal =
      (fun object_ ->
        let rec collect one fields = function
          | [] -> (
              match (one, fields) with
              | None, [] -> Snapshot.object_from_owned []
              | Some (name, value), [] ->
                  Snapshot.singleton_object_from_owned name value
              | None, fields -> Snapshot.object_from_owned fields
              | Some _, _ -> assert false)
          | (_, Error error) :: _ -> Error error
          | (name, Ok value) :: rest -> (
              match (one, fields) with
              | None, [] -> collect (Some (name, value)) [] rest
              | Some field, [] -> collect None [ (name, value); field ] rest
              | None, fields -> collect None ((name, value) :: fields) rest
              | Some _, _ -> assert false)
        in
        {
          fragment = collect None [] object_.fields;
          has_error = object_.has_error;
        });
  }

let untyped_patch_fragment (patch : untyped_patch) = patch.fragment
let untyped_patch_has_error (patch : untyped_patch) = patch.has_error

let untyped_patch_of_value = function
  | Value.Object _ as value ->
      { fragment = Value.freeze value; has_error = false }
  | _ ->
      invalid_arg
        "Observe.Generated_runtime.untyped_value_patch: expected object"

let builder =
  {
    text =
      (fun ~tag format ->
        Format.kasprintf (fun message -> Text { tag; message }) format);
    untyped = untyped_builder.untyped;
    field = untyped_builder.field;
    object_ = untyped_builder.object_;
    seal = (fun object_ -> Untyped (untyped_builder.seal object_).fragment);
    value = (fun value -> Value value);
    error =
      (fun interpretation ?backtrace error ->
        Untyped (Error.freeze interpretation ?backtrace error));
    typed = (fun description value -> Typed (description, value));
  }
