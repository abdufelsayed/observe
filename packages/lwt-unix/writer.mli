type t
type offer = Accepted | Full | Closed

type delivery_facts =
  | No_problem
  | Rejected
  | Delivery_lost
  | Rejected_and_lost

val create : capacity:int -> Lwt_unix.file_descr -> t

val offer : t -> string -> offer
(** Submit one indivisible record without blocking the producer. *)

val delivery_facts : t -> delivery_facts
(** The finite cumulative delivery facts observed by this writer. *)

val flush : t -> unit Lwt.t
(** Resolve when every record accepted before the call has been written. *)

val shutdown : t -> unit Lwt.t
(** Stop accepting records, drain those already accepted, and stop the worker.
    Repeated calls share the same completion. *)

val abort : t -> unit
(** Synchronously discard a writer created for an initialization which did not
    publish. No records may have been offered. *)
