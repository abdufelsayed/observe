type clock_error = Unavailable
type console_acceptance = Accepted | Rejected
type 'a outcome = Returned of 'a | Raised of exn * Printexc.raw_backtrace

module type S = sig
  type +'a t
  type state
  type 'a key

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val observe : (unit -> 'a t) -> 'a outcome t
  val repropagate : exn -> Printexc.raw_backtrace -> 'a t
  val create_key : unit -> 'a key
  val get : state -> 'a key -> 'a option
  val with_binding : state -> 'a key -> 'a -> (unit -> 'b t) -> 'b t
  val protect : state -> finally:(unit -> unit) -> (unit -> 'a t) -> 'a t
  val is_control_exception : state -> exn -> bool

  module Clock : sig
    val now : state -> (Timestamp.t, clock_error) result
    val monotonic_now : state -> (int64, clock_error) result
  end

  module Identity : sig
    val next : state -> (string, clock_error) result
  end

  module Console : sig
    val style : state -> Formatter.style
    val offer : state -> string -> console_acceptance
  end
end
