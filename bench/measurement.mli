type configuration = { quota_ms : int; limit : int; allocation_runs : int }

type t = {
  name : string;
  suite : string;
  boundary : string;
  payload : string;
  nanoseconds_per_operation : float;
  operations_per_second : float;
  minor_bytes_per_operation : float;
  major_bytes_per_operation : float;
  promoted_bytes_per_operation : float;
  retained_bytes : float option;
  minor_collections_per_operation : float;
  major_collections_per_operation : float;
  r_squared : float option;
  samples : int;
  measured_nanoseconds : int64;
}

val run :
  configuration -> Scenario.t -> (unit -> unit) -> (unit -> float option) -> t
