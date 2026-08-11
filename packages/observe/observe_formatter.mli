(** Pure projections of completed represented logs. *)

type error = Invalid_utf8 | Non_finite_float | Unsupported_value | Failed

type style =
  | Plain
  | Ansi_16
  | Ansi_256
  | Truecolor
      (** Presentation capability for readable terminal output. Colored styles
          share one semantic palette and differ only in available color depth;
          they do not change layout or semantic information. *)

type t

val create : (Observe_log.t -> (string, error) result) -> t
(** Construct a formatter. The callback must perform no I/O. *)

val format : t -> Observe_log.t -> (string, error) result
(** Invoke a formatter without catching callback exceptions. The engine owns
    exception containment so it can preserve its narrow exception policy. *)

val readable : style -> t
(** Render compact tagged text or an ordered structured tree. Timestamps use UTC
    time of day with millisecond precision. Caller-controlled terminal control
    characters are escaped. *)

val json : t
val json_lines : t
