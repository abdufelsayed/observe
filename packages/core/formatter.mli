(** Pure projections of completed represented logs. *)

type error = Formatter_intf.error =
  | Invalid_utf8
  | Non_finite_float
  | Unsupported_value
  | Failed

type style = Formatter_intf.style =
  | Plain
  | Ansi_16
  | Ansi_256
  | Truecolor
      (** Presentation capability for pretty console output. Colored styles
          share one semantic palette and differ only in available color depth;
          they do not change layout or semantic information. *)

type t

val create : (Log.t -> (string, error) result) -> t
(** Construct a formatter. The callback must perform no I/O. *)

val format : t -> Log.t -> (string, error) result
(** Invoke a formatter without catching callback exceptions. The engine owns
    exception containment so it can preserve its narrow exception policy. *)

val pretty : style -> t
(** Render compact tagged text or an ordered structured tree. Timestamps use UTC
    time of day with millisecond precision. Caller-controlled terminal control
    characters are escaped. *)

val pretty_line : style -> t
(** Internal terminal projection with exactly one trailing line feed. *)

val json : t

val ndjson : t
(** One compact JSON object followed by one line feed. *)
