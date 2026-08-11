(** Unix platform support for Observe.

    Most Lwt applications should initialize logging through [Observe_lwt_unix].
    Integration authors can use {!Platform} with [Observe.Runtime.Make]. *)

module Platform : Observe.Platform.S with type t = unit
(** The Observe platform mechanism backed by the OS wall clock and the process
    standard-error descriptor.

    The best supported truecolor, 256-color, or 16-color presentation is
    selected from passive terminal signals when standard error is a TTY.
    [NO_COLOR], [TERM=dumb], redirected output, and probe failure select plain
    output. Detection never queries terminal input.

    Terminal writes are synchronous. [Accepted] means that every supplied byte
    was handed to the descriptor. It does not promise atomicity, ordering,
    flushing, or durability. *)
