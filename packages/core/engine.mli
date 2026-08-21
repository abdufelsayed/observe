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
  console:(string -> Io.console_acceptance) ->
  is_control_exception:(exn -> bool) ->
  t

val create_capture :
  Config.t ->
  clock:(unit -> (Timestamp.t, Io.clock_error) result) ->
  monotonic_now:(unit -> (int64, Io.clock_error) result) ->
  next_id:(unit -> (string, Io.clock_error) result) ->
  is_control_exception:(exn -> bool) ->
  Capture.t ->
  t

val after_install : t -> unit
(** Record installation-only diagnostics after this engine wins publication. *)

val emit_point : t -> Level.t -> Message.author -> unit

type wide
type contribution = { body : Snapshot.t; has_error : bool }

val inert_wide : unit -> wide
val create_wide : t -> name:string -> origin:Log.structured_origin -> wide

val contribute_wide :
  wide -> (unit -> (contribution, Snapshot.error) result) -> unit

val set_wide_level : wide -> Level.t -> unit
val emit_wide : wide -> unit
