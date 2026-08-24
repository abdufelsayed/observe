(** A completed authoring result. [Engine] invokes an [author] only after
    admission, then seals the returned message with runtime metadata. *)

type piece =
  | Typed_value : 'a Type.t * 'a -> piece
  | Object of object_
  | Untyped_value of Value.t
  | Interpreted_error :
      'error Error.t * Printexc.raw_backtrace option * 'error
      -> piece

and object_ = { fields : (string * piece) list; has_error : bool }

type field = { name : string; piece : piece; has_error : bool }
type untyped_patch = { piece : piece; has_error : bool }
type untyped = piece

type t =
  | Text of { tag : string; message : string }
  | Untyped of untyped
  | Typed : ('a, 'builder) Schema.t * 'a -> t

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

let ( |+ ) object_ field =
  {
    fields = (field.name, field.piece) :: object_.fields;
    has_error = object_.has_error || field.has_error;
  }

let rec freeze_piece context ~depth = function
  | Typed_value (description, value) ->
      Type.freeze_into description context ~depth value
  | Object object_ -> freeze_object context ~depth object_
  | Untyped_value value -> Value.freeze_into value context ~depth
  | Interpreted_error (interpretation, backtrace, error) ->
      Error.freeze_into interpretation ?backtrace error context ~depth

and freeze_object context ~depth object_ =
  let rec collect fields = function
    | [] -> Snapshot.object_ context ~depth fields
    | (name, piece) :: rest -> (
        match freeze_piece context ~depth:(depth + 1) piece with
        | Error _ as error -> error
        | Ok value -> collect ((name, value) :: fields) rest)
  in
  collect [] object_.fields

let materialize piece =
  let context = Snapshot.create_context () in
  match freeze_piece context ~depth:0 piece with
  | Ok value -> Ok (Snapshot.seal context value)
  | Error _ as error -> error

let rec untyped_builder =
  {
    untyped = { fields = []; has_error = false };
    field =
      (fun name description value ->
        { name; piece = Typed_value (description, value); has_error = false });
    object_ =
      (fun name author ->
        let patch = author untyped_builder in
        { name; piece = patch.piece; has_error = patch.has_error });
    error =
      (fun ~using ?backtrace error ->
        {
          piece = Interpreted_error (using, backtrace, error);
          has_error = true;
        });
    seal =
      (fun object_ -> { piece = Object object_; has_error = object_.has_error });
  }

let materialize_untyped_patch (patch : untyped_patch) = materialize patch.piece
let materialize_untyped value = materialize value
let untyped_patch_has_error (patch : untyped_patch) = patch.has_error

let untyped_patch_of_value = function
  | Value.Object _ as value ->
      { piece = Untyped_value value; has_error = false }
  | _ ->
      invalid_arg
        "Observe.Ppx_runtime.Logs.untyped_value_patch: expected object"

let untyped_message_of_value = function
  | Value.Object _ as value -> Untyped (Untyped_value value)
  | _ -> invalid_arg "Observe.Ppx_runtime.Logs.untyped_message: expected object"

let builder =
  {
    text =
      (fun ~tag format ->
        Format.kasprintf (fun message -> Text { tag; message }) format);
    untyped = untyped_builder.untyped;
    field = untyped_builder.field;
    object_ = untyped_builder.object_;
    seal = (fun object_ -> Untyped (Object object_));
    error =
      (fun ~using ?backtrace error ->
        Untyped (Interpreted_error (using, backtrace, error)));
    typed = (fun ~using value -> Typed (using, value));
  }
