(** A pending authoring message. [Logs] constructs values, [Observer] routes
    them, and [Engine] consumes them; expensive construction stays deferred
    until after admission. *)

type t =
  | Text of { tag : string; message : string }
  | Lazy_text of { tag : string; message : unit -> string }
  | Free of Value.t
  | Lazy_free of (unit -> Value.t)
  | Structured : 'a Type.t * 'a -> t
  | Lazy_structured : 'a Type.t * (unit -> 'a) -> t

let text ~tag message = Text { tag; message }
let text_lazy ~tag message = Lazy_text { tag; message }
let free value = Free value
let free_lazy make = Lazy_free make
let structured description value = Structured (description, value)
let structured_lazy description make = Lazy_structured (description, make)
