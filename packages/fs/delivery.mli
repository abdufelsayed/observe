module Make (IO : Io.S) : sig
  type error =
    | Invalid_path
    | Invalid_capacity of int
    | Io of IO.error
    | Zero_progress
    | Invalid_write_count of int
    | Unexpected of exn

  type t

  val create : path:string -> ?capacity:int -> unit -> (t, error) result IO.t
  val drain : t -> Observe.Drain.t
  val flush : t -> (unit, error) result IO.t
  val shutdown : t -> (unit, error) result IO.t
  val pp_error : Format.formatter -> error -> unit
end
