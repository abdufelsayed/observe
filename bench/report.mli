type metadata
type summary

val metadata :
  commit:string ->
  suite:string ->
  repetitions:int ->
  Measurement.configuration ->
  metadata

val summarize : Measurement.t list -> summary
val print_table : summary list -> unit

val print_comparison :
  metadata:metadata ->
  baselines:string list ->
  allow_scenario_drift:bool ->
  summary list ->
  unit

val write_json : path:string -> metadata -> summary list -> unit
