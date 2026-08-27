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

type sampling_draw = unit -> float
(** A synchronous source of independent probabilistic draws. Each call must
    return a finite value greater than or equal to zero and less than one.
    Observe serializes a configured custom source. Raised exceptions and invalid
    results are contained and cause the affected log to be retained. A source
    that recursively emits a sampled Observe log is rejected instead of
    deadlocking. *)

val init :
  ?id_generator:id_generator ->
  ?sampling_draw:sampling_draw ->
  Observe.Config.t ->
  (unit, Observe.init_error) result
(** Install the process-wide Observe engine with Lwt dynamic context, the OS
    wall clock, cryptographically random UUID v4 operation identities, and
    automatic standard-error output. Configured sampling uses runtime-owned
    random draws. Custom identity and sampling sources are useful for
    deterministic tests or application-specific policy. Importing this package
    allocates no writer or background work. See {!type:id_generator} and
    {!type:sampling_draw} for their contracts. Returns [Error Runtime_closed]
    after shutdown has begun. *)

val init_exn :
  ?id_generator:id_generator ->
  ?sampling_draw:sampling_draw ->
  Observe.Config.t ->
  unit
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
    call have reached their effect boundary. On an incomplete finite wait or
    participant outcome, raises [Lifecycle.Incomplete report]; preserves
    [Lwt.Canceled]. A complete report means all participants settled without a
    reported problem, but only to their owned effect boundaries, not durability.
    This uses a 30-second finite caller wait; timed-out shared work continues.
    Ordinary application-defined drains are not registered automatically. *)

val shutdown : unit -> unit Lwt.t
(** Close production logging admission, drain accepted records, and stop every
    output worker. On an incomplete finite wait or participant outcome, raises
    [Lifecycle.Incomplete report]; preserves [Lwt.Canceled]. A complete report
    means all participants settled without a reported problem, but only to their
    owned effect boundaries, not durability. This uses a 30-second finite caller
    wait; timed-out shared work continues. Repeated calls share one idempotent
    state machine. Shutdown is terminal even when called before initialization.
    Logging after shutdown does not evaluate author callbacks. *)

module Lifecycle : sig
  (** Expert registration for independently installed Lwt-Unix outputs. Reports
      distinguish rejected, lost, failed, timed-out, and cancelled delivery;
      problem output labels are bounded runtime identities. Duration bounds are
      finite caller waits only; shared work continues after timeout. *)

  type error = Closed

  module Duration : sig
    type t
    type error = Negative | Non_finite

    val create : seconds:float -> (t, error) result
    (** Rejects negative and non-finite durations; zero is valid. *)

    exception Invalid_duration of error
    (** Raised by {!create_exn} with the typed reason for invalid input. *)

    val create_exn : seconds:float -> t
  end

  type problem =
    | Rejected of { output : string }
    | Delivery_lost of { output : string }
    | Destination_failed of { output : string }
    | Timed_out of { output : string }
    | Cancelled of { output : string }

  type report
  (** An aggregate of hook settlement and bounded runtime-identity problems.
      [complete report] means every participant settled without a reported
      problem at its owned effect boundary; it does not assert durability.
      Runtime control exceptions remain native rather than becoming reports. *)

  val complete : report -> bool

  val problems : report -> problem list
  (** Participant problems, identified by bounded runtime output labels. *)

  exception Incomplete of report
  (** Raised by ordinary {!flush} and {!shutdown} when finite waiting or a
      participant outcome is incomplete. *)

  module Integration : sig
    type facts = No_problem | Rejected | Delivery_lost | Rejected_and_lost
    type error = Closed | Invalid_label

    val register :
      label:string ->
      facts:(unit -> facts) ->
      flush:(unit -> unit Lwt.t) ->
      shutdown:(unit -> unit Lwt.t) ->
      (unit, error) result
    (** Register a ready output with a bounded safe label and finite cumulative
        delivery facts. Fact callbacks are sampled synchronously when lifecycle
        reports are produced, including timeout snapshots, and must be prompt,
        side-effect free, and concurrency-safe. Independent flush boundaries may
        overlap each other and an in-progress shutdown; repeated shutdown
        callers share one shutdown invocation. Runtime control exceptions remain
        native. *)
  end

  val flush : ?within:Duration.t -> unit -> report Lwt.t
  (** Run an independent flush boundary and leave the lifecycle open. The finite
      [within] bounds this caller's waiter only; cancellation preserves
      [Lwt.Canceled], while shared work continues. *)

  val shutdown : ?within:Duration.t -> unit -> report Lwt.t
  (** Start or join the one shared idempotent shutdown; [within] bounds this
      caller's waiter only, shared work continues, and shutdown closes admission
      permanently. *)

  val register :
    flush:(unit -> unit Lwt.t) ->
    shutdown:(unit -> unit Lwt.t) ->
    (unit, error) result
  (** Join the process lifecycle while it is open. Registered hooks are owned
      until process shutdown. Every hook is attempted even when another hook
      fails. Independent flush boundaries may overlap each other and an
      in-progress shutdown, so callbacks must be prompt and concurrency-safe;
      repeated shutdown callers share one shutdown invocation. Runtime control
      exceptions remain native. *)
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
