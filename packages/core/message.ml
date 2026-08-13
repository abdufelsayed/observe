(** A completed authoring result. [Engine] invokes an [author] only after
    admission, then seals the returned message with runtime metadata. *)

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

let builder =
  {
    text =
      (fun ~tag format ->
        Format.kasprintf (fun message -> Text { tag; message }) format);
    untyped = (fun value -> Untyped value);
    typed = (fun description value -> Typed (description, value));
  }
