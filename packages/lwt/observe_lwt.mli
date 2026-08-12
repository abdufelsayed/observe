(** Configurable Lwt I/O for Observe.

    Most Lwt applications on Unix should initialize logging through
    [Observe_lwt_unix]. Integration authors can inject another clock and console
    implementation with {!create}, then apply [Observe.Make] to {!IO}. *)

type t
(** A completed Lwt I/O state. *)

val create :
  clock:(unit -> (Observe.Instant.t, Observe.IO.clock_error) result) ->
  console_style:(unit -> Observe.Formatter.style) ->
  write_console:(string -> Observe.IO.console_acceptance) ->
  unit ->
  t
(** Complete the Lwt implementation with an application-owned clock and console.
    Construction performs no I/O and does not initialize Observe. *)

(** Observe effects backed by Lwt promises and callback-local bindings.

    Dynamic bindings propagate through Lwt callbacks registered inside their
    scope. They do not propagate through [Lwt_preemptive.detach], OS threads, or
    another I/O implementation. Native [Lwt.Canceled] remains control flow. *)
module IO : Observe.IO.S with type 'a t = 'a Lwt.t and type state = t
