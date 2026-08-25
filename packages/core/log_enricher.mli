(** Validated semantic enrichment selected by logging configuration. *)

type t
(** One named lazy contribution to an admitted observation. *)

type error
(** A deterministic construction error. *)

exception Invalid_enricher of error

val create :
  name:string ->
  ?authoritative_fields:string list ->
  (unit -> Value.t) ->
  (t, error) result
(** [create ~name ?authoritative_fields author] validates and retains one lazy
    contribution without invoking [author].

    [name] must be non-empty valid UTF-8. Authoritative field names must also be
    non-empty valid UTF-8, unique within the enricher, and outside Observe's
    reserved envelope. [author] is synchronous and may run concurrently for
    different observations; it must be safe for concurrent invocation and must
    not recursively emit Observe logs. *)

val create_exn :
  name:string -> ?authoritative_fields:string list -> (unit -> Value.t) -> t
(** Like {!create}, but raises [Invalid_enricher error]. *)

val name : t -> string
val authoritative_fields : t -> string list
val is_authoritative : t -> string -> bool

val author : t -> unit -> Value.t
(** Return the retained callback without invoking it. Engine code owns lazy
    evaluation and exception containment. *)

val pp_error : Format.formatter -> error -> unit
