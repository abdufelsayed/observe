(** Ready Observe composition for Lwt applications on Unix.

    Initialize once at the application composition root. Ordinary application
    and library code then logs through [Observe.Logs]. Console delivery is
    bounded and serialized by one Lwt worker. *)

val init : Observe.Config.t -> (unit, Observe.init_error) result
(** Install the process-wide Observe engine with Lwt dynamic context, the OS
    wall clock, and automatic standard-error output. *)

val init_exn : Observe.Config.t -> unit
(** Like {!init}, but raises [Observe.Init_error] on failure. *)

val flush : unit -> unit Lwt.t
(** Resolve when all console records accepted before the call have reached
    standard error. This does not flush application-defined drains. *)

val shutdown : unit -> unit Lwt.t
(** Stop console acceptance, drain accepted records, and stop the output worker.
    Repeated calls share the same completion. Logging after shutdown diagnoses
    console rejection while configured drains continue independently. *)

module Test : sig
  exception Capture_error of Observe.capture_error
  (** A capture could not start. The callback is not called. *)

  val with_capture :
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
