(** Runtime and platform composition for the process-wide logging route. *)

module Platform : sig
  type clock_error = Unavailable
  type console_acceptance = Accepted | Rejected

  module type S = sig
    type t

    val console_style : t -> Observe_formatter.style
    val now : t -> (Observe_instant.t, clock_error) result
    val write_console : t -> string -> console_acceptance
  end
end

module type S = sig
  type +'a t
  type context
  type 'a key

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val create_key : unit -> 'a key
  val get : context -> 'a key -> 'a option
  val with_binding : context -> 'a key -> 'a -> (unit -> 'b t) -> 'b t
  val protect : context -> finally:(unit -> unit) -> (unit -> 'a t) -> 'a t
  val is_control_exception : context -> exn -> bool
end

type init_error = Already_initialized | Runtime_already_registered
type capture_error = Runtime_already_registered | Invalid_capacity of int

exception Init_error of init_error

module Make (Runtime : S) (Platform : Platform.S) : sig
  type +'a io = 'a Runtime.t
  type t

  val create : runtime_context:Runtime.context -> platform:Platform.t -> t
  val init : t -> Observe_config.t -> (unit, init_error) result
  val init_exn : t -> Observe_config.t -> unit

  val with_capture :
    t ->
    Observe_config.t ->
    ?capacity:int ->
    (Observe_capture.t -> 'a io) ->
    ('a, capture_error) result io
end

val emit : level:Observe_level.t -> Observe_engine.message -> unit
(** Internal entry point for the static [Observe_logs] API. *)
