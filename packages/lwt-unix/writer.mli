type t
type offer = Accepted | Full | Closed

val create : capacity:int -> Lwt_unix.file_descr -> t

val offer : t -> string -> offer
(** Submit one indivisible record without blocking the producer. *)

val flush : t -> unit Lwt.t
(** Resolve when every record accepted before the call has been written. *)

val shutdown : t -> unit Lwt.t
(** Stop accepting records, drain those already accepted, and stop the worker.
    Repeated calls share the same completion. *)

val abort : t -> unit
(** Synchronously discard a writer created for an initialization which did not
    publish. No records may have been offered. *)
