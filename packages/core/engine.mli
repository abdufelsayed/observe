(** Private portable logging engine. *)

type 'a contained = Returned of 'a | Raised

val contain : is_control_exception:(exn -> bool) -> (unit -> 'a) -> 'a contained
(** Contain an ordinary callback exception. Resource exhaustion, [Sys.Break],
    and runtime-classified control exceptions are re-raised with their original
    backtrace. A raising classifier is itself a defect: the original exception
    is then re-raised unchanged rather than replaced. *)

type t

val create_outputs :
  Config.t ->
  console_style:Formatter.style ->
  clock:(unit -> (Timestamp.t, Io.clock_error) result) ->
  monotonic_now:(unit -> (int64, Io.clock_error) result) ->
  next_id:(unit -> (string, Io.clock_error) result) ->
  resolve_operation:(unit -> Log.operation_reference option) ->
  console:(string -> Io.console_acceptance) ->
  is_control_exception:(exn -> bool) ->
  t

val create_capture :
  Config.t ->
  clock:(unit -> (Timestamp.t, Io.clock_error) result) ->
  monotonic_now:(unit -> (int64, Io.clock_error) result) ->
  next_id:(unit -> (string, Io.clock_error) result) ->
  resolve_operation:(unit -> Log.operation_reference option) ->
  is_control_exception:(exn -> bool) ->
  Capture.t ->
  t

val after_install : t -> unit
(** Record installation-only diagnostics after this engine wins publication. *)

val close : t -> unit
(** Stop production admission and delivery. Capture engines remain controlled by
    their lexical capture scope. *)

val record_diagnostic : t -> Diagnostics.kind -> unit
(** Record through the engine's active output or capture diagnostic boundary. *)

val emit_point :
  t -> ?correlation:Log.operation_reference -> Level.t -> Message.author -> unit

type wide
type current = Open of wide | Typed of wide * Schema.identity

val current_reference : current -> Log.operation_reference option
val current_wide : current -> wide

type contribution =
  | Contribution of Snapshot.fragment * bool
  | Invalid_contribution of Snapshot.error

val inert_wide : unit -> wide

val create_wide :
  t ->
  ?parent:wide ->
  name:string ->
  origin:Log.structured_origin ->
  unit ->
  wide

val wide_reference : wide -> Log.operation_reference option
val wide_limits : wide -> Log_limits.t
val contribute_wide : wide -> (unit -> contribution) -> bool
val annotate_wide : wide -> Level.t -> (unit -> string) -> bool
val set_wide_level : wide -> Level.t -> unit
val emit_wide : wide -> unit
