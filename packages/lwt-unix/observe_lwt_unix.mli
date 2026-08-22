(** Ready Observe composition for Lwt programs on Unix.

    Initialize once at the process composition root. All caller code then logs
    through [Observe.Logs]. Console delivery is bounded and serialized by one
    Lwt worker. *)

val init : Observe.Config.t -> (unit, Observe.init_error) result
(** Install the process-wide Observe engine with Lwt dynamic context, the OS
    wall clock, and automatic standard-error output. *)

val init_exn : Observe.Config.t -> unit
(** Like {!init}, but raises [Observe.Init_error] on failure. *)

val with_wide :
  ('builder, 'patch) Observe.Logs.t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
(** Bind an existing wide log for scoped point-log correlation. This does not
    catch errors or emit the wide log. *)

val manage :
  ('builder, 'patch) Observe.Logs.t ->
  error:exn Observe.Error.t ->
  (unit -> 'a Lwt.t) ->
  'a Lwt.t
(** Run and complete one existing wide log while preserving the exact Lwt
    result, exception, backtrace, or cancellation. *)

val fork :
  parent:('parent_builder, 'parent_patch) Observe.Logs.t ->
  name:string ->
  error:exn Observe.Error.t ->
  ((Observe.Logs.untyped_builder, Observe.Logs.untyped_patch) Observe.Logs.t ->
  'a Lwt.t) ->
  'a Lwt.t
(** Run one managed independent untyped child and restore the parent scope. *)

val fork_typed :
  parent:('parent_builder, 'parent_patch) Observe.Logs.t ->
  name:string ->
  ('record, 'builder) Observe.Schema.t ->
  error:exn Observe.Error.t ->
  (('builder, 'record Observe.Schema.patch) Observe.Logs.t -> 'a Lwt.t) ->
  'a Lwt.t
(** Run one managed independent schema-locked child. *)

val flush : unit -> unit Lwt.t
(** Resolve when all console and registered output records accepted before the
    call have reached their effect boundary. Ordinary application-defined drains
    are not registered automatically. *)

val shutdown : unit -> unit Lwt.t
(** Stop console and registered output acceptance, drain accepted records, and
    stop every output worker. Repeated calls share the same completion. Logging
    after shutdown diagnoses output rejection. *)

module Lifecycle : sig
  (** Expert registration for independently installed Lwt-Unix outputs. *)

  type error = Closed

  val register :
    flush:(unit -> unit Lwt.t) ->
    shutdown:(unit -> unit Lwt.t) ->
    (unit, error) result
  (** Join the process lifecycle while it is open. Registered hooks are owned
      until process shutdown. Every hook is attempted even when another hook
      fails. *)
end

module Test : sig
  exception Capture_error of Observe.capture_error
  (** A capture could not start. The callback is not called. *)

  val with_capture_exn :
    Observe.Config.t ->
    ?capacity:int ->
    (Observe.Capture.t -> 'a Lwt.t) ->
    'a Lwt.t
  (** Run [callback] with an isolated Lwt-scoped capture of ordinary
      [Observe.Logs] calls.

      The inner scope wins when scopes are nested. The prior scope is restored
      after success, exception, or [Lwt.Canceled]. A callback registered in the
      scope but run after closure cannot fall through to production. Bindings do
      not propagate through [Lwt_preemptive.detach], OS threads, or another I/O
      implementation. *)
end
