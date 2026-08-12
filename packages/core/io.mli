(** Effects required by the portable Observe core. *)

type clock_error = Engine.clock_error = Unavailable
type console_acceptance = Engine.console_acceptance = Accepted | Rejected

module type S = sig
  type +'a t
  type state
  type 'a key

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val create_key : unit -> 'a key
  val get : state -> 'a key -> 'a option
  val with_binding : state -> 'a key -> 'a -> (unit -> 'b t) -> 'b t
  val protect : state -> finally:(unit -> unit) -> (unit -> 'a t) -> 'a t
  val is_control_exception : state -> exn -> bool

  module Clock : sig
    val now : state -> (Instant.t, clock_error) result
  end

  module Console : sig
    val style : state -> Formatter.style
    val write : state -> string -> console_acceptance
  end
end
