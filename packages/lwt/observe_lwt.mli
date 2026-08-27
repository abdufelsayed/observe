(** Configurable Lwt I/O for Observe.

    Most Lwt applications on Unix should initialize logging through
    [Observe_lwt_unix]. Alternative clock and console implementations can be
    supplied with {!create}, then composed by applying [Observe.Make] to {!IO}.
*)

type t
(** A completed Lwt I/O state. *)

val create :
  clock:(unit -> (Observe.Timestamp.t, Observe.IO.clock_error) result) ->
  monotonic_now:(unit -> (int64, Observe.IO.clock_error) result) ->
  next_id:(unit -> (string, Observe.IO.clock_error) result) ->
  sampling_draw:(unit -> float) ->
  create_stable_sampling_draw:(unit -> unit -> float) ->
  console_style:(unit -> Observe.Formatter.style) ->
  offer_console:(string -> Observe.IO.console_acceptance) ->
  can_lookup_context:(unit -> bool) ->
  unit ->
  t
(** Complete the Lwt implementation with composition-provided wall and monotonic
    clocks, occurrence identity, sampling draws, and console capabilities.
    [sampling_draw] must promptly return a finite value greater than or equal to
    zero and less than one, and must be safe for every execution context exposed
    by the composition. [create_stable_sampling_draw] must allocate a
    concurrency-safe one-shot callback without drawing; that callback invokes
    the source at most once and replays its value or exception with the original
    backtrace to every caller. Core contains and diagnoses ordinary source
    failures while preserving runtime control exceptions. [can_lookup_context]
    must return [false] outside the scheduler execution context because Lwt's
    implicit callback storage is process-global and not thread-safe. A
    single-threaded runtime can return [true]. Construction performs no I/O and
    does not initialize Observe. *)

(** Observe effects backed by Lwt promises and callback-local bindings.

    Dynamic bindings propagate through Lwt callbacks registered inside their
    scope. They do not propagate through [Lwt_preemptive.detach], OS threads, or
    another I/O implementation. Native [Lwt.Canceled] remains control flow. *)
module IO : Observe.IO.S with type 'a t = 'a Lwt.t and type state = t
