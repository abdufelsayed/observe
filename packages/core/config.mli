(** Validated process-wide logging behavior. *)

type t
type console = Auto | Pretty | Ndjson | Silent
type field = Service | Environment | Version | Enrichers

type problem =
  | Empty
  | Invalid_utf8
  | Duplicate_enricher_name of string
  | Overlapping_authoritative_field of string

type error = { field : field; problem : problem }

exception Invalid_configuration of error

val create :
  service:string ->
  ?environment:string ->
  ?version:string ->
  ?enabled:bool ->
  ?console:console ->
  ?min_level:Level.t ->
  ?drains:Drain.t list ->
  ?enrichers:Log_enricher.t list ->
  ?limits:Log_limits.t ->
  ?redaction:Log_redaction.t ->
  unit ->
  (t, error) result
(** Construct validated logging behavior.

    [service] is required. Service, environment, and version values must be
    non-empty valid UTF-8. [Auto] selects pretty output when [environment] is
    absent, [dev], or [development], and NDJSON otherwise. [Pretty], [Ndjson],
    and [Silent] override that selection. Other defaults are [enabled=true],
    [console=Auto], [min_level=Info], no drains, no enrichers, and finite
    default logging limits. *)

val create_exn :
  service:string ->
  ?environment:string ->
  ?version:string ->
  ?enabled:bool ->
  ?console:console ->
  ?min_level:Level.t ->
  ?drains:Drain.t list ->
  ?enrichers:Log_enricher.t list ->
  ?limits:Log_limits.t ->
  ?redaction:Log_redaction.t ->
  unit ->
  t
(** Like {!create}, but raises [Invalid_configuration error]. *)

val service : t -> string
val environment : t -> string option
val version : t -> string option
val enabled : t -> bool
val console : t -> console
val min_level : t -> Level.t
val drains : t -> Drain.t list
val enrichers : t -> Log_enricher.t list
val limits : t -> Log_limits.t
val redaction : t -> Log_redaction.t
val pp_error : Format.formatter -> error -> unit
