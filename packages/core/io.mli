(** Effects required by the portable Observe core. This module owns the
    port/result contract; the engine consumes it. *)

type clock_error = Unavailable
type console_acceptance = Accepted | Rejected

module type S = sig
  type +'a t
  type state
  type 'a key

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val create_key : unit -> 'a key
  (** Return a fresh generative dynamic-context key. *)

  val get : state -> 'a key -> 'a option
  (** Read only the binding associated with the supplied key. Must be safe to
      call from a foreign OS thread that has no runtime context, returning
      [None] there. *)

  val with_binding : state -> 'a key -> 'a -> (unit -> 'b t) -> 'b t
  (** Restore the previous binding after success, exception, or native
      cancellation. *)

  val protect : state -> finally:(unit -> unit) -> (unit -> 'a t) -> 'a t
  (** Run [finally] exactly once after the callback settles. When [finally]
      returns normally, preserve the callback's result, exception, or native
      cancellation. [finally] is an integration cleanup hook and must not raise;
      a raising cleanup follows the runtime's native protection semantics. *)

  val is_control_exception : state -> exn -> bool
  (** Identify native cancellation and other control-flow exceptions that the
      core must preserve rather than contain. A raising classifier never causes
      the core to replace the original exception. *)

  module Clock : sig
    val now : state -> (Timestamp.t, clock_error) result
    (** Return wall-clock epoch time. [Unavailable] means that no timestamp can
        be supplied. Ordinary exceptions are diagnosed by the core. *)

    val monotonic_now : state -> (int64, clock_error) result
    (** Return a process-relative monotonic nanosecond value. The value is used
        only to compute elapsed time and is never presented as wall time. *)
  end

  module Identity : sig
    val next : state -> (string, clock_error) result
    (** Return a non-empty identifier for one active wide-log occurrence. *)
  end

  module Console : sig
    val style : state -> Formatter.style
    (** Report the console's maximum supported presentation capability. Return
        [Plain] when support is unknown. This query must not raise. *)

    val offer : state -> string -> console_acceptance
    (** Accept one completely formatted record exactly as supplied. The core
        owns record termination. [Accepted] promises immediate handoff only, not
        flushing or durability. Ordinary exceptions are diagnosed by the core.
    *)
  end
end
