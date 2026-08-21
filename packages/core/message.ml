(** A completed authoring result. [Engine] invokes an [author] only after
    admission, then seals the returned message with runtime metadata. *)

type t =
  | Text of { tag : string; message : string }
  | Untyped of Value.t
  | Typed : ('a, 'builder) Schema.t * 'a -> t

type object_ = { fields : (string * Value.t) list; has_error : bool }
type field = { name : string; value : Value.t; has_error : bool }
type open_patch = { value : Value.t; has_error : bool }

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

let ( |+ ) object_ field =
  {
    fields = (field.name, field.value) :: object_.fields;
    has_error = object_.has_error || field.has_error;
  }

let rec open_builder =
  {
    untyped = { fields = []; has_error = false };
    field =
      (fun name description value ->
        { name; value = Value.embed description value; has_error = false });
    object_ =
      (fun name author ->
        let patch = author open_builder in
        { name; value = patch.value; has_error = patch.has_error });
    error =
      (fun interpretation ?backtrace error ->
        {
          value = Error.value interpretation ?backtrace error;
          has_error = true;
        });
    seal =
      (fun object_ ->
        {
          value = Value.object_ (List.rev object_.fields);
          has_error = object_.has_error;
        });
  }

let open_patch_value (patch : open_patch) = patch.value
let open_patch_has_error (patch : open_patch) = patch.has_error

let open_patch_of_value = function
  | Value.Object _ as value -> { value; has_error = false }
  | _ ->
      invalid_arg "Observe.Generated_runtime.open_value_patch: expected object"

let builder =
  {
    text =
      (fun ~tag format ->
        Format.kasprintf (fun message -> Text { tag; message }) format);
    untyped = open_builder.untyped;
    field = open_builder.field;
    object_ = open_builder.object_;
    seal = (fun object_ -> Untyped (open_builder.seal object_).value);
    value = (fun value -> Untyped value);
    error =
      (fun interpretation ?backtrace error ->
        Untyped (Error.value interpretation ?backtrace error));
    typed = (fun description value -> Typed (description, value));
  }
