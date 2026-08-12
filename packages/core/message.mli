type t =
  | Text of { tag : string; message : string }
  | Lazy_text of { tag : string; message : unit -> string }
  | Free of Value.t
  | Lazy_free of (unit -> Value.t)
  | Structured : 'a Type.t * 'a -> t
  | Lazy_structured : 'a Type.t * (unit -> 'a) -> t

val text : tag:string -> string -> t
val text_lazy : tag:string -> (unit -> string) -> t
val free : Value.t -> t
val free_lazy : (unit -> Value.t) -> t
val structured : 'a Type.t -> 'a -> t
val structured_lazy : 'a Type.t -> (unit -> 'a) -> t
