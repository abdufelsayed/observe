(** Validated process-wide logging behavior. *)

type t
type field = Service | Environment | Version
type problem = Empty | Invalid_utf8
type error = { field : field; problem : problem }

exception Invalid_configuration of error

val create :
  service:string ->
  ?environment:string ->
  ?version:string ->
  ?enabled:bool ->
  ?pretty:bool ->
  ?silent:bool ->
  ?min_level:Observe_level.t ->
  ?drains:Observe_drain.t list ->
  unit ->
  (t, error) result
(** Construct validated logging behavior.

    [service] is required. Service, environment, and version values must be
    non-empty valid UTF-8. If [pretty] is absent, readable output is selected
    when [environment] is absent, [dev], or [development]; other environments
    select JSON. An explicit [pretty] value overrides this selection. Other
    defaults are [enabled=true], [silent=false], [min_level=Info], and no
    drains. *)

val create_exn :
  service:string ->
  ?environment:string ->
  ?version:string ->
  ?enabled:bool ->
  ?pretty:bool ->
  ?silent:bool ->
  ?min_level:Observe_level.t ->
  ?drains:Observe_drain.t list ->
  unit ->
  t
(** Like {!create}, but raises [Invalid_configuration error]. *)

val service : t -> string
val environment : t -> string option
val version : t -> string option
val enabled : t -> bool
val pretty : t -> bool
val silent : t -> bool
val min_level : t -> Observe_level.t
val drains : t -> Observe_drain.t list
val pp_error : Format.formatter -> error -> unit
