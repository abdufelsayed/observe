type t = {
  max_depth : int;
  max_object_fields : int;
  max_collection_length : int;
  max_string_bytes : int;
  max_bytes_length : int;
  max_nodes : int;
  max_total_bytes : int;
}

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

(* These finite defaults preserve the established canonical safety envelope.
   The P16 benchmark suite measures representative work below them and
   controlled truncation at tighter boundaries; keeping the envelope avoids an
   arbitrary compatibility change while every configured path remains
   unconditionally bounded. They are policy defaults, not a claim about exact
   heap bytes or a universally optimal workload. *)
let default =
  {
    max_depth = 64;
    max_object_fields = 1_024;
    max_collection_length = 1_024;
    max_string_bytes = 1_048_576;
    max_bytes_length = 1_048_576;
    max_nodes = 100_000;
    max_total_bytes = 4_194_304;
  }

let validate field value =
  if value > 0 then Ok () else Error { field; value; problem = Non_positive }

let first_error checks =
  let rec loop = function
    | [] -> Ok ()
    | check :: rest -> (
        match check () with Ok () -> loop rest | Error _ as error -> error)
  in
  loop checks

let create ?(max_depth = default.max_depth)
    ?(max_object_fields = default.max_object_fields)
    ?(max_collection_length = default.max_collection_length)
    ?(max_string_bytes = default.max_string_bytes)
    ?(max_bytes_length = default.max_bytes_length)
    ?(max_nodes = default.max_nodes)
    ?(max_total_bytes = default.max_total_bytes) () =
  match
    first_error
      [
        (fun () -> validate Max_depth max_depth);
        (fun () -> validate Max_object_fields max_object_fields);
        (fun () -> validate Max_collection_length max_collection_length);
        (fun () -> validate Max_string_bytes max_string_bytes);
        (fun () -> validate Max_bytes_length max_bytes_length);
        (fun () -> validate Max_nodes max_nodes);
        (fun () -> validate Max_total_bytes max_total_bytes);
      ]
  with
  | Error _ as error -> error
  | Ok () ->
      Ok
        {
          max_depth;
          max_object_fields;
          max_collection_length;
          max_string_bytes;
          max_bytes_length;
          max_nodes;
          max_total_bytes;
        }

let create_exn ?max_depth ?max_object_fields ?max_collection_length
    ?max_string_bytes ?max_bytes_length ?max_nodes ?max_total_bytes () =
  match
    create ?max_depth ?max_object_fields ?max_collection_length
      ?max_string_bytes ?max_bytes_length ?max_nodes ?max_total_bytes ()
  with
  | Ok limits -> limits
  | Error error -> raise (Invalid_limits error)

let max_depth limits = limits.max_depth
let max_object_fields limits = limits.max_object_fields
let max_collection_length limits = limits.max_collection_length
let max_string_bytes limits = limits.max_string_bytes
let max_bytes_length limits = limits.max_bytes_length
let max_nodes limits = limits.max_nodes
let max_total_bytes limits = limits.max_total_bytes

let with_max_total_bytes limits max_total_bytes =
  { limits with max_total_bytes }

let pp_field formatter = function
  | Max_depth -> Format.pp_print_string formatter "max_depth"
  | Max_object_fields -> Format.pp_print_string formatter "max_object_fields"
  | Max_collection_length ->
      Format.pp_print_string formatter "max_collection_length"
  | Max_string_bytes -> Format.pp_print_string formatter "max_string_bytes"
  | Max_bytes_length -> Format.pp_print_string formatter "max_bytes_length"
  | Max_nodes -> Format.pp_print_string formatter "max_nodes"
  | Max_total_bytes -> Format.pp_print_string formatter "max_total_bytes"

let pp_problem formatter = function
  | Non_positive -> Format.pp_print_string formatter "must be positive"

let pp_error formatter { field; value; problem } =
  Format.fprintf formatter "%a=%d %a" pp_field field value pp_problem problem
