(** Portable structured logging.

    The core owns logging policy and performs no I/O. Integrations provide one
    completed {!module-type:IO.S} implementation. Ordinary application code logs
    through {!Logs}. *)

(** Type descriptions used by typed structured logs. Each description keeps
    Repr's machine representation together with Observe's presentation
    semantics. {!Type.repr} is the explicit interoperability escape hatch. *)
module Type : sig
  type 'a t

  val repr : 'a t -> 'a Repr.t
  (** Recover the underlying Repr description for machine operations and
      interoperability. *)

  val of_repr : ?json:(Buffer.t -> 'a -> unit) -> 'a Repr.t -> 'a t
  (** Lift a raw Repr description. The optional writer appends exactly one
      compact JSON value and must agree with the representation accepted by the
      Repr decoder. Without it, JSON and pretty formatting use the generic
      compatibility path and cannot recover distinctions erased by that
      encoding. Fully described {!Type} combinators and descriptions produced by
      [@@deriving observe] use direct Observe writers instead and retain those
      distinctions. *)

  type len = Repr.len

  val unit : unit t
  val bool : bool t
  val char : char t
  val int : int t
  val int32 : int32 t
  val int63 : Optint.Int63.t t
  val int64 : int64 t
  val float : float t
  val string : string t
  val bytes : bytes t
  val string_of : len -> string t
  val bytes_of : len -> bytes t
  val boxed : 'a t -> 'a t
  val list : ?len:len -> 'a t -> 'a list t
  val array : ?len:len -> 'a t -> 'a array t
  val option : 'a t -> 'a option t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val result : 'a t -> 'b t -> ('a, 'b) result t
  val seq : 'a t -> 'a Seq.t t
  val ref : 'a t -> 'a ref t
  val lazy_t : 'a t -> 'a Lazy.t t
  val queue : 'a t -> 'a Queue.t t
  val stack : 'a t -> 'a Stack.t t
  val hashtbl : 'key t -> 'value t -> ('key, 'value) Hashtbl.t t

  type empty = Repr.empty = |

  val empty : empty t

  type ('a, 'b, 'c) open_record
  type ('a, 'b) field

  val record : string -> 'b -> ('a, 'b, 'b) open_record
  val field : string -> 'a t -> ('b -> 'a) -> ('b, 'a) field

  val ( |+ ) :
    ('a, 'b, 'c -> 'd) open_record -> ('a, 'c) field -> ('a, 'b, 'd) open_record

  val sealr : ('a, 'b, 'a) open_record -> 'a t

  type ('a, 'b, 'c) open_variant
  type ('a, 'b) case
  type 'a case_p

  val variant : string -> 'b -> ('a, 'b, 'b) open_variant

  val case0 :
    ?polymorphic:bool ->
    ?is:('a -> bool) ->
    string ->
    'a ->
    ('a, 'a case_p) case

  val case1 :
    ?polymorphic:bool ->
    ?project:('a -> 'b option) ->
    string ->
    'b t ->
    ('b -> 'a) ->
    ('a, 'b -> 'a case_p) case

  val ( |~ ) :
    ('a, 'b, 'c -> 'd) open_variant ->
    ('a, 'c) case ->
    ('a, 'b, 'd) open_variant

  val sealv : ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t
  val enum : string -> (string * 'a) list -> 'a t
  val mu : ('a t -> 'a t) -> 'a t
  val mu2 : ('a t -> 'b t -> 'a t * 'b t) -> 'a t * 'b t

  val to_json_string : 'a t -> 'a -> string
  (** Allocate one compact JSON value using the description's attached writer.
  *)

  val of_json_string : 'a t -> string -> ('a, [ `Msg of string ]) result
  val map : 'b t -> ('b -> 'a) -> ('a -> 'b) -> 'a t
end

module Generated_runtime : sig
  (** Compatibility contract for code emitted by [observe.ppx]. Application code
      should use {!Type}; this module may only evolve with a coordinated
      PPX/runtime compatibility change. *)

  type 'a description = 'a Type.t
  type renderer
  type placement

  type rendered =
    | Scalar of (renderer -> unit)
    | Node of (renderer -> placement -> unit)

  val with_json : 'a description -> (Buffer.t -> 'a -> unit) -> 'a description
  val with_plan : 'a description -> ('a -> rendered) -> 'a description

  val with_recursive_plan :
    'a description -> ('a description -> 'a -> rendered) -> 'a description

  val json : 'a description -> Buffer.t -> 'a -> unit
  val plan : 'a description -> 'a -> rendered
  val is_scalar : 'a description -> 'a -> bool
  val render : 'a description -> renderer -> placement -> 'a -> unit
  val inline : placement
  val start : renderer -> placement -> scalar:bool -> bool
  val finish : renderer -> bool -> unit

  val field :
    'a description -> renderer -> last:bool -> name:string -> 'a -> unit

  val constructor :
    'a description -> renderer -> last:bool -> name:string -> 'a -> unit

  val constructor_start :
    renderer -> last:bool -> name:string -> scalar:bool -> bool

  val variant : renderer -> placement -> polymorphic:bool -> string -> unit
  val variant_label : renderer -> polymorphic:bool -> string -> unit
  val empty_record : renderer -> unit
  val json_unit : Buffer.t -> unit -> unit
  val json_bool : Buffer.t -> bool -> unit
  val json_char : Buffer.t -> char -> unit
  val json_int : Buffer.t -> int -> unit
  val json_int32 : Buffer.t -> int32 -> unit
  val json_int64 : Buffer.t -> int64 -> unit
  val json_float : Buffer.t -> float -> unit
  val json_string : Buffer.t -> string -> unit
  val json_bytes : Buffer.t -> bytes -> unit
  val json_list : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a list -> unit
  val json_array : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a array -> unit
  val json_option : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a option -> unit
  val json_field : 'a description -> Buffer.t -> bool -> string -> 'a -> bool
end

module Level : sig
  type t =
    | Debug
    | Info
    | Warn
    | Error  (** Ordered as [Debug < Info < Warn < Error]. *)

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val t : t Type.t
end

module Timestamp : sig
  type t
  (** A wall-clock occurrence timestamp expressed as Unix nanoseconds. *)

  val of_unix_ns : int64 -> t
  val to_unix_ns : t -> int64
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val t : t Type.t
end

module Value : sig
  type t = private
    | Null
    | Bool of bool
    | Int of int
    | Float of float
    | String of string
    | List of t list
    | Object of (string * t) list
    | Embedded : 'a Type.t * 'a -> t
        (** An untyped structured value. The outer tree is immutable; values
            added with {!embed} are retained by reference and may themselves be
            mutable. The private constructors support stable structural
            inspection while the functions below remain the construction API. *)

  val null : t
  val bool : bool -> t
  val int : int -> t
  val float : float -> t
  val string : string -> t
  val option : t option -> t
  val list : t list -> t
  val object_ : (string * t) list -> t

  val embed : 'a Type.t -> 'a -> t
  (** Preserve a typed value and its description without eager rendering. *)

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string

  type json_error = Invalid_utf8 | Non_finite_float | Unsupported_value

  val to_json_string : t -> (string, json_error) result
  (** Encode one compact JSON value with Observe's direct writer. *)
end

module Log : sig
  type t
  (** A completed admitted log. Typed values and values embedded in untyped
      bodies are retained by reference, not deeply copied. *)

  type body =
    | Text of { tag : string; message : string }
    | Untyped of Value.t
    | Typed : 'a Type.t * 'a -> body
        (** The three semantic body forms. Pattern matching is safe because this
            type carries no completed-log invariant. *)

  val service : t -> string
  val environment : t -> string option
  val version : t -> string option
  val timestamp : t -> Timestamp.t
  val level : t -> Level.t
  val body : t -> body
end

module Diagnostics : sig
  type kind =
    | Not_initialized
    | No_delivery_target
    | Capture_lookup_raised
    | Clock_unavailable
    | Clock_raised
    | Message_evaluation_raised
    | Formatting_failed
    | Formatting_raised
    | Console_rejected
    | Console_raised
    | Drain_rejected
    | Drain_raised
    | Drain_delivery_failed
    | Capture_overflow
    | Capture_closed

  type entry = private { kind : kind; count : int }

  val snapshot : unit -> entry list
  (** A non-clearing, finite process snapshot in stable order. *)
end

module Drain : sig
  type acceptance = Accepted | Rejected
  type t

  val create : (Log.t -> acceptance) -> t
  (** Construct an additional output with a synchronous submission callback.

      [Accepted] means immediate ownership acceptance only. A drain retaining
      work after return must first copy or project everything it needs. Ordinary
      callback exceptions are contained as [Diagnostics.Drain_raised]; runtime
      control exceptions are preserved. *)

  module Integration : sig
    val report_failure : unit -> unit
    (** Report that asynchronous work accepted by a drain later failed. This
        increments one bounded, non-recursive process diagnostic and performs no
        logging, callback, or I/O. *)
  end
end

module Formatter : sig
  type error = Invalid_utf8 | Non_finite_float | Unsupported_value | Failed

  type style =
    | Plain
    | Ansi_16
    | Ansi_256
    | Truecolor
        (** Colored styles share one semantic palette and differ only in
            available color depth. They do not change layout or semantic
            information. *)

  type t

  val create : (Log.t -> (string, error) result) -> t

  val format : t -> Log.t -> (string, error) result
  (** Invoke a formatter. Callback exceptions remain exceptions; the logging
      engine contains them at the application boundary. *)

  val pretty : style -> t
  (** Render compact tagged text or an ordered structured tree with UTC
      millisecond timestamps and console-safe caller data. *)

  val json : t

  val ndjson : t
  (** One compact JSON object followed by one line feed. *)
end

module Capture : sig
  type t

  val default_capacity : int

  val logs : t -> Log.t list
  (** Retained logs in admission order. Typed values remain by-reference values,
      not deep snapshots. *)

  val diagnostics : t -> Diagnostics.entry list
end

module Config : sig
  type t
  type console = Auto | Pretty | Ndjson | Silent
  type field = Service | Environment | Version
  type problem = Empty | Invalid_utf8
  type error = { field : field; problem : problem }

  exception Invalid_configuration of error

  val create :
    service:string ->
    ?environment:string ->
    ?version:string ->
    ?enabled:bool ->
    ?console:console ->
    ?min_level:Level.t ->
    ?drains:Drain.t list ->
    unit ->
    (t, error) result
  (** Construct validated logging behavior. [Auto] selects pretty output when
      [environment] is absent, [dev], or [development], and NDJSON otherwise.
      The other console policies explicitly override that selection. *)

  val create_exn :
    service:string ->
    ?environment:string ->
    ?version:string ->
    ?enabled:bool ->
    ?console:console ->
    ?min_level:Level.t ->
    ?drains:Drain.t list ->
    unit ->
    t
  (** Like {!create}, but raises [Invalid_configuration error]. *)

  val service : t -> string
  val environment : t -> string option
  val version : t -> string option
  val enabled : t -> bool
  val console : t -> console
  val min_level : t -> Level.t
  val drains : t -> Drain.t list
  val pp_error : Format.formatter -> error -> unit
end

module Logs : sig
  (** Process-wide admission-first logging. Every level function checks the
      active route and configured level before invoking its authoring callback:

      {[
      Observe.Logs.info (fun m ->
          m.text ~tag:"auth" "user %d logged in" user_id)
      ]} *)

  type message
  (** A completed authoring result produced inside an admitted callback. *)

  type builder = private {
    text :
      'a. tag:string -> ('a, Format.formatter, unit, message) format4 -> 'a;
    untyped : Value.t -> message;
    typed : 'a. 'a Type.t -> 'a -> message;
  }
  (** The admitted message builder. [text] supports type-safe format strings;
      [untyped] accepts a dynamic value; [typed] retains an OCaml value with its
      type description. *)

  type author = builder -> message
  (** Message authoring invoked only after route and level admission. *)

  val emit : level:Level.t -> author -> unit
  (** Emit through the active scoped or production route. Before installation,
      messages are withheld and diagnosed. Logging does not raise merely because
      process initialization has not happened. Ordinary authoring exceptions are
      withheld and diagnosed as [Diagnostics.Message_evaluation_raised]. *)

  val debug : author -> unit
  val info : author -> unit
  val warn : author -> unit
  val error : author -> unit
end

module IO : sig
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
    (** Read only the binding associated with the supplied key. *)

    val with_binding : state -> 'a key -> 'a -> (unit -> 'b t) -> 'b t
    (** Restore the previous binding after success, exception, or native
        cancellation. *)

    val protect : state -> finally:(unit -> unit) -> (unit -> 'a t) -> 'a t
    (** Run [finally] exactly once after the callback settles. When [finally]
        returns normally, preserve the callback's result, exception, or native
        cancellation. The cleanup hook must not raise. *)

    val is_control_exception : state -> exn -> bool
    (** Identify native cancellation and other control-flow exceptions that the
        core must preserve rather than contain. *)

    module Clock : sig
      val now : state -> (Timestamp.t, clock_error) result
      (** Return wall-clock epoch time. [Unavailable] means that no timestamp
          can be supplied. Ordinary exceptions are diagnosed by the core. *)
    end

    module Console : sig
      val style : state -> Formatter.style
      (** Report the console's maximum supported presentation capability. Return
          [Plain] when support is unknown. This query must not raise. *)

      val offer : state -> string -> console_acceptance
      (** Write one completely formatted record exactly as supplied. The core
          owns record termination. [Accepted] promises immediate handoff only,
          not flushing or durability. Ordinary exceptions are diagnosed by the
          core. *)
    end
  end
end

type init_error = Already_initialized | IO_already_registered
type capture_error = IO_already_registered | Invalid_capacity of int

exception Init_error of init_error

module Make (IO : IO.S) : sig
  type +'a io = 'a IO.t
  type t

  val create : IO.state -> t
  (** Create an inert observer from a completed I/O state. This does not
      register or initialize the process route. *)

  val init : t -> Config.t -> (unit, init_error) result

  val init_exn : t -> Config.t -> unit
  (** [init_exn] raises [Init_error error] when [init] returns [Error error]. *)

  val with_capture :
    t ->
    Config.t ->
    ?capacity:int ->
    (Capture.t -> 'a io) ->
    ('a, capture_error) result io
  (** Run with capture as the innermost dynamic route.

      Registration and capacity errors occur before [callback]. The capture
      suppresses production delivery for the dynamic extent. Prior bindings are
      restored and the capture is closed exactly once after callback success,
      exception, or cancellation. The callback outcome is preserved; a retained
      capture remains available for inspection but closed to further delivery.
  *)
end
