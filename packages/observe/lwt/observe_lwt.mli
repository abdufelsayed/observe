(** Lwt runtime support for Observe.

    Most Lwt applications on Unix should initialize logging through
    [Observe_lwt_unix]. Integration authors can use {!Runtime} with
    [Observe.Runtime.Make]. *)

(** The Observe runtime mechanism backed by Lwt promises and implicit callback
    arguments.

    Dynamic bindings propagate through Lwt callbacks registered inside their
    scope. They do not propagate through [Lwt_preemptive.detach], OS threads, or
    another runtime. *)
module Runtime :
  Observe.Runtime.S with type 'a t = 'a Lwt.t and type context = unit
