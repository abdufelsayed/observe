module Make (IO : Io.S) : sig
  type error =
    | Invalid_directory
    | Invalid_capacity of int
    | Io of IO.error
    | Zero_progress
    | Invalid_write_count of int
    | Unexpected of exn

  type t

  type delivery_facts =
    | No_problem
    | Rejected
    | Delivery_lost
    | Rejected_and_lost

  val create : dir:string -> ?capacity:int -> unit -> (t, error) result IO.t
  val drain : t -> Observe.Drain.t

  val delivery_facts : t -> delivery_facts
  (** The finite cumulative delivery facts observed by this writer. *)

  val flush : t -> (unit, error) result IO.t
  val shutdown : t -> (unit, error) result IO.t
  val pp_error : Format.formatter -> error -> unit
end
