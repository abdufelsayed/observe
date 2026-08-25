(** Finite bounds for canonical logging values. *)

type t

type field =
  | Max_depth
  | Max_object_fields
  | Max_collection_length
  | Max_string_bytes
  | Max_bytes_length
  | Max_nodes
  | Max_total_bytes

type problem = Non_positive
type error = { field : field; value : int; problem : problem }

exception Invalid_limits of error

val default : t

val create :
  ?max_depth:int ->
  ?max_object_fields:int ->
  ?max_collection_length:int ->
  ?max_string_bytes:int ->
  ?max_bytes_length:int ->
  ?max_nodes:int ->
  ?max_total_bytes:int ->
  unit ->
  (t, error) result

val create_exn :
  ?max_depth:int ->
  ?max_object_fields:int ->
  ?max_collection_length:int ->
  ?max_string_bytes:int ->
  ?max_bytes_length:int ->
  ?max_nodes:int ->
  ?max_total_bytes:int ->
  unit ->
  t

val max_depth : t -> int
val max_object_fields : t -> int
val max_collection_length : t -> int
val max_string_bytes : t -> int
val max_bytes_length : t -> int
val max_nodes : t -> int
val max_total_bytes : t -> int

val with_max_total_bytes : t -> int -> t
(** Internal policy adjustment after required completed-log metadata has been
    accounted. The caller must supply a positive bound. *)

val pp_error : Format.formatter -> error -> unit
