(** Ready Observe composition for Lwt programs on Unix.

    Initialize once at the process composition root. All caller code then logs
    through [Observe.Logs]. Console delivery is bounded and serialized by one
    Lwt worker. *)

val init : Observe.Config.t -> (unit, Observe.init_error) result
(** Install the process-wide Observe engine with Lwt dynamic context, the OS
    wall clock, and automatic standard-error output. *)

val init_exn : Observe.Config.t -> unit
(** Like {!init}, but raises [Observe.Init_error] on failure. *)

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
