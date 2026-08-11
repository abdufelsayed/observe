(** Private portable logging engine. *)

type message =
  | Text of { tag : string; message : string }
  | Lazy_text of { tag : string; message : unit -> string }
  | Free of (unit -> Observe_value.t)
  | Structured : 'a Repr.t * 'a -> message

val text : tag:string -> string -> message
val text_lazy : tag:string -> (unit -> string) -> message
val free : (unit -> Observe_value.t) -> message
val structured : 'a Repr.t -> 'a -> message

type clock_error = Unavailable
type terminal_acceptance = Accepted | Rejected
type 'a contained = Returned of 'a | Raised

val contain : is_control_exception:(exn -> bool) -> (unit -> 'a) -> 'a contained
(** Contain an ordinary callback exception. Resource exhaustion, [Sys.Break],
    and runtime-classified control exceptions are re-raised with their original
    backtrace. *)

type t

val create_production :
  Observe_config.t ->
  terminal_style:Observe_formatter.style ->
  clock:(unit -> (Observe_instant.t, clock_error) result) ->
  terminal:(string -> terminal_acceptance) ->
  is_control_exception:(exn -> bool) ->
  t

val create_capture :
  Observe_config.t ->
  clock:(unit -> (Observe_instant.t, clock_error) result) ->
  is_control_exception:(exn -> bool) ->
  Observe_capture.t ->
  t

val after_production_install : t -> unit
(** Record installation-only diagnostics after this engine wins publication. *)

val emit : t -> Observe_level.t -> message -> unit
