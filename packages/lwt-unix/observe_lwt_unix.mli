(** Ready Observe composition for Lwt applications on Unix.

    Initialize once at the application composition root. Ordinary application
    and library code then logs through [Observe.Logs]. *)

val init : Observe.Config.t -> (unit, Observe.init_error) result
(** Install the process-wide Observe engine with Lwt dynamic context, the OS
    wall clock, and automatic standard-error output. *)

val init_exn : Observe.Config.t -> unit
(** Like {!init}, but raises [Observe.Init_error] on failure. *)

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
