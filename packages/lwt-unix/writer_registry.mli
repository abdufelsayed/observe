type error = Closed
type t
type identity

module Delivery_facts : sig
  type t = No_problem | Rejected | Delivery_lost | Rejected_and_lost
end

type delivery_facts = Delivery_facts.t
type integration_error = Closed | Invalid_label

type problem =
  | Rejected of { output : string }
  | Delivery_lost of { output : string }
  | Destination_failed of { output : string }
  | Timed_out of { output : string }
  | Cancelled of { output : string }

type report

val report_complete : report -> bool
val report_problems : report -> problem list
val create : unit -> t
val create_identity : ?name:string -> unit -> identity
val valid_label : string -> bool

val register :
  t ->
  identity:identity ->
  flush:(unit -> unit Lwt.t) ->
  shutdown:(unit -> unit Lwt.t) ->
  (unit, error) result

val register_integration :
  t ->
  label:string ->
  facts:(unit -> delivery_facts) ->
  flush:(unit -> unit Lwt.t) ->
  shutdown:(unit -> unit Lwt.t) ->
  (unit, integration_error) result

val identity_name : identity -> string
val shutdown_report : t -> report Lwt.t
val flush_within : t -> float -> report Lwt.t
val shutdown_within : t -> float -> report Lwt.t
