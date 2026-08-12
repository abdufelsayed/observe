type metadata

val metadata :
  commit:string -> suite:string -> Measurement.configuration -> metadata

val print_table : Measurement.t list -> unit
val print_comparison : baseline:string -> Measurement.t list -> unit
val write_json : path:string -> metadata -> Measurement.t list -> unit
