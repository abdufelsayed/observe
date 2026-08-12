(** Private portable logging engine. *)

type message =
  | Text of { tag : string; message : string }
  | Lazy_text of { tag : string; message : unit -> string }
  | Free of (unit -> Value.t)
  | Structured : 'a Type.t * 'a -> message

val text : tag:string -> string -> message
val text_lazy : tag:string -> (unit -> string) -> message
val free : (unit -> Value.t) -> message
val structured : 'a Type.t -> 'a -> message

type clock_error = Unavailable
type console_acceptance = Accepted | Rejected
type 'a contained = Returned of 'a | Raised

val contain : is_control_exception:(exn -> bool) -> (unit -> 'a) -> 'a contained
(** Contain an ordinary callback exception. Resource exhaustion, [Sys.Break],
    and runtime-classified control exceptions are re-raised with their original
    backtrace. *)

type t

val create_production :
  Config.t ->
  console_style:Formatter.style ->
  clock:(unit -> (Instant.t, clock_error) result) ->
  console:(string -> console_acceptance) ->
  is_control_exception:(exn -> bool) ->
  t

val create_capture :
  Config.t ->
  clock:(unit -> (Instant.t, clock_error) result) ->
  is_control_exception:(exn -> bool) ->
  Capture.t ->
  t

val after_production_install : t -> unit
(** Record installation-only diagnostics after this engine wins publication. *)

val emit : t -> Level.t -> message -> unit
