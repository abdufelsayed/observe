(** Ready Observe composition for Lwt programs on Unix.

    Initialize once at the process composition root. All caller code then logs
    through [Observe.Logs]. Console delivery is bounded and serialized by one
    Lwt worker. *)

type id_generator = unit -> string
(** A synchronous source of fresh operation occurrence identifiers.

    Observe serializes calls to the configured generator. Each call must return
    a fresh, non-empty, valid UTF-8 identifier and should complete promptly.
    Raised exceptions and invalid results are contained and diagnosed; the
    affected operation is not published. *)

val init :
  ?id_generator:id_generator ->
  Observe.Config.t ->
  (unit, Observe.init_error) result
(** Install the process-wide Observe engine with Lwt dynamic context, the OS
    wall clock, cryptographically random UUID v4 operation identities, and
    automatic standard-error output. A custom generator replaces only operation
    identity and is useful for deterministic tests or an application-specific
    identity policy. Importing this package allocates no writer or background
    work. See {!type:id_generator} for its contract. Returns
    [Error Runtime_closed] after shutdown has begun. *)

val init_exn : ?id_generator:id_generator -> Observe.Config.t -> unit
(** Like {!init}, but raises [Observe.Init_error] on failure. *)

val with_operation :
  name:string ->
  ?using:('record, 'builder) Observe.Schema.t ->
  ?error:exn Observe.Error.t ->
  (unit -> 'a Lwt.t) ->
  'a Lwt.t
(** Run one scoped wide operation, make it current during [callback], and make
    one final publication attempt on success, exception, or cancellation.
    Escaping exceptions are contributed using [error], which defaults to
    {!Observe.Error.exn}, then re-raised with their original backtrace. Failed
    error interpretation seals and withholds the invalid observation. *)

val fork :
  name:string ->
  ?using:('record, 'builder) Observe.Schema.t ->
  ?error:exn Observe.Error.t ->
  (unit -> 'a Lwt.t) ->
  'a Lwt.t
(** Run one scoped, independently emitted child of the current operation. The
    child records its parent's complete reference; it does not copy or modify
    the parent's event. The prior current operation is restored when [callback]
    finishes. Raises [Observe.Logs.Current_error Not_bound] outside an operation
    scope. *)

val flush : unit -> unit Lwt.t
(** Resolve when all console and registered output records accepted before the
    call have reached their effect boundary. Ordinary application-defined drains
    are not registered automatically. *)

val shutdown : unit -> unit Lwt.t
(** Close production logging admission, drain accepted records, and stop every
    output worker. Repeated calls share the same completion. Shutdown is
    terminal even when called before initialization. Logging after shutdown does
    not evaluate author callbacks. *)

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
    config:Observe.Config.t ->
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
