(** Runtime-neutral structured logging.

    The core owns logging policy and performs no I/O. Runtime adapters provide
    dynamic context; platform adapters provide the clock and console output.
    Ordinary application code logs through {!Logs}. *)

(** Runtime descriptions used by typed structured logs. Each description keeps
    Repr's machine representation together with Observe's presentation
    semantics. {!Type.repr} is the explicit interoperability escape hatch. *)
module Type : sig
  type 'a t

  val repr : 'a t -> 'a Repr.t
  (** Recover the underlying Repr description for machine operations and
      interoperability. *)

  val of_repr : 'a Repr.t -> 'a t
  (** Lift a raw Repr description. Readable formatting then uses Repr's JSON
      projection and cannot recover distinctions erased by that encoding.
      Descriptions produced by [@@deriving observe] retain those distinctions.
  *)

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
  val case0 : string -> 'a -> ('a, 'a case_p) case
  val case1 : string -> 'b t -> ('b -> 'a) -> ('a, 'b -> 'a case_p) case

  val ( |~ ) :
    ('a, 'b, 'c -> 'd) open_variant ->
    ('a, 'c) case ->
    ('a, 'b, 'd) open_variant

  val sealv : ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t
  val enum : string -> (string * 'a) list -> 'a t
  val mu : ('a t -> 'a t) -> 'a t
  val mu2 : ('a t -> 'b t -> 'a t * 'b t) -> 'a t * 'b t

  type +'a staged = 'a Repr.staged
  type 'a equal = 'a Repr.equal
  type 'a compare = 'a Repr.compare
  type 'a pp = 'a Repr.pp
  type 'a of_string = 'a Repr.of_string
  type 'a encode_json = 'a Repr.encode_json
  type 'a decode_json = 'a Repr.decode_json
  type 'a encode_bin = 'a Repr.encode_bin
  type 'a decode_bin = 'a Repr.decode_bin
  type -'a size_of = 'a Repr.size_of
  type 'a impl = 'a Repr.impl = Structural | Custom of 'a | Undefined

  val stage : 'a -> 'a staged
  val unstage : 'a staged -> 'a
  val equal : 'a t -> 'a equal staged
  val compare : 'a t -> 'a compare staged
  val pp : 'a t -> 'a pp
  val pp_dump : 'a t -> 'a pp
  val to_string : 'a t -> 'a -> string
  val of_string : 'a t -> 'a of_string
  val encode_json : 'a t -> Jsonm.encoder -> 'a -> unit
  val decode_json : 'a t -> Jsonm.decoder -> ('a, [ `Msg of string ]) result

  val decode_json_lexemes :
    'a t -> Jsonm.lexeme list -> ('a, [ `Msg of string ]) result

  val to_json_string : ?minify:bool -> 'a t -> 'a -> string
  val of_json_string : 'a t -> string -> ('a, [ `Msg of string ]) result
  val encode_bin : 'a t -> 'a encode_bin staged
  val decode_bin : 'a t -> 'a decode_bin staged
  val to_bin_string : 'a t -> ('a -> string) staged
  val of_bin_string : 'a t -> (string -> ('a, [ `Msg of string ]) result) staged
  val size_of : 'a t -> ('a -> int option) staged

  val like :
    ?pp:'a pp ->
    ?of_string:'a of_string ->
    ?json:'a encode_json * 'a decode_json ->
    ?bin:'a encode_bin * 'a decode_bin * 'a size_of ->
    ?unboxed_bin:'a encode_bin * 'a decode_bin * 'a size_of ->
    ?equal:'a equal ->
    ?compare:'a compare ->
    ?short_hash:(?seed:int -> 'a -> int) ->
    ?pre_hash:'a encode_bin ->
    'a t ->
    'a t

  val partially_abstract :
    pp:'a pp impl ->
    of_string:'a of_string impl ->
    json:('a encode_json * 'a decode_json) impl ->
    bin:('a encode_bin * 'a decode_bin * 'a size_of) impl ->
    unboxed_bin:('a encode_bin * 'a decode_bin * 'a size_of) impl ->
    equal:'a equal impl ->
    compare:'a compare impl ->
    short_hash:(?seed:int -> 'a -> int) impl ->
    pre_hash:'a encode_bin impl ->
    'a t ->
    'a t

  val map :
    ?pp:'a pp ->
    ?of_string:'a of_string ->
    ?json:'a encode_json * 'a decode_json ->
    ?bin:'a encode_bin * 'a decode_bin * 'a size_of ->
    ?unboxed_bin:'a encode_bin * 'a decode_bin * 'a size_of ->
    ?equal:'a equal ->
    ?compare:'a compare ->
    ?short_hash:(?seed:int -> 'a -> int) ->
    ?pre_hash:'a encode_bin ->
    'b t ->
    ('b -> 'a) ->
    ('a -> 'b) ->
    'a t

  module For_ppx : sig
    type view
    (** Runtime support for code generated by [observe.ppx]. Application code
        should use the ordinary description combinators above. *)

    type error
    type result = (view, error) Stdlib.result

    val with_present : 'a t -> ('a -> result) -> 'a t
    val present : 'a t -> 'a -> result
    val record : (string * result) list -> result
    val list : result list -> result
    val list_map : ('a -> result) -> 'a list -> result
    val option : result option -> result
    val variant : polymorphic:bool -> string -> result option -> result
  end
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

module Instant : sig
  type t
  (** A wall-clock occurrence instant expressed as epoch nanoseconds. *)

  val of_epoch_nanoseconds : int64 -> t
  val to_epoch_nanoseconds : t -> int64
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val t : t Type.t
end

module Value : sig
  type t
  (** A free-form structured value. The outer tree is immutable; values added
      with {!embed} are retained by reference and may themselves be mutable. *)

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
end

module Log : sig
  type t
  (** A completed admitted log. Structured payloads and typed values embedded in
      free-form payloads are retained by reference, not deeply copied. *)

  type payload =
    | Text of { tag : string; message : string }
    | Free of Value.t
    | Structured : 'a Type.t * 'a -> payload
        (** The three semantic payload forms. Pattern matching is safe because
            this type carries no completed-log invariant. *)

  val service : t -> string
  val environment : t -> string option
  val version : t -> string option
  val instant : t -> Instant.t
  val level : t -> Level.t
  val payload : t -> payload
end

module Diagnostics : sig
  type kind =
    | Not_initialized
    | No_output
    | Scope_raised
    | Clock_unavailable
    | Clock_raised
    | Authoring_raised
    | Formatting_failed
    | Formatting_raised
    | Console_rejected
    | Console_raised
    | Drain_rejected
    | Drain_raised
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

  val readable : style -> t
  (** Render compact tagged text or an ordered structured tree with UTC
      millisecond timestamps and console-safe caller data. *)

  val json : t
  val json_lines : t
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
  type field = Service | Environment | Version
  type problem = Empty | Invalid_utf8
  type error = { field : field; problem : problem }

  exception Invalid_configuration of error

  val create :
    service:string ->
    ?environment:string ->
    ?version:string ->
    ?enabled:bool ->
    ?pretty:bool ->
    ?silent:bool ->
    ?min_level:Level.t ->
    ?drains:Drain.t list ->
    unit ->
    (t, error) result
  (** Construct validated logging behavior. If [pretty] is absent, readable
      output is selected when [environment] is absent, [dev], or [development];
      other environments select JSON. An explicit [pretty] value overrides this
      selection. *)

  val create_exn :
    service:string ->
    ?environment:string ->
    ?version:string ->
    ?enabled:bool ->
    ?pretty:bool ->
    ?silent:bool ->
    ?min_level:Level.t ->
    ?drains:Drain.t list ->
    unit ->
    t
  (** Like {!create}, but raises [Invalid_configuration error]. *)

  val service : t -> string
  val environment : t -> string option
  val version : t -> string option
  val enabled : t -> bool
  val pretty : t -> bool
  val silent : t -> bool
  val min_level : t -> Level.t
  val drains : t -> Drain.t list
  val pp_error : Format.formatter -> error -> unit
end

module Logs : sig
  type message
  (** A pending message. Expensive free-form construction remains deferred until
      after admission. *)

  val text : tag:string -> string -> message

  val text_lazy : tag:string -> (unit -> string) -> message
  (** Construct text only after admission. Ordinary exceptions are withheld and
      diagnosed as [Diagnostics.Authoring_raised]. *)

  val free : (unit -> Value.t) -> message
  val structured : 'a Type.t -> 'a -> message

  val emit : level:Level.t -> message -> unit
  (** Emit through the active scoped or production route. Before installation,
      messages are withheld and diagnosed. Logging does not raise merely because
      process initialization has not happened. *)

  val debug : message -> unit
  val info : message -> unit
  val warn : message -> unit
  val error : message -> unit
end

module Platform : sig
  type clock_error = Unavailable
  type console_acceptance = Accepted | Rejected

  module type S = sig
    type t

    val console_style : t -> Formatter.style
    (** Report the console's maximum supported presentation capability. Return
        [Plain] when support is unknown. This query must not raise. *)

    val now : t -> (Instant.t, clock_error) result
    (** Return wall-clock epoch time. [Unavailable] means that no timestamp can
        be supplied. Ordinary exceptions are diagnosed by the core. *)

    val write_console : t -> string -> console_acceptance
    (** Write one completely formatted record exactly as supplied. The core owns
        record termination. [Accepted] promises immediate handoff only, not
        flushing or durability. Ordinary exceptions are diagnosed by the core.
    *)
  end
end

module Runtime : sig
  module type S = sig
    type +'a t
    type context
    type 'a key

    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t

    val create_key : unit -> 'a key
    (** Return a fresh generative dynamic-context key. *)

    val get : context -> 'a key -> 'a option
    (** Read only the binding associated with the supplied key. *)

    val with_binding : context -> 'a key -> 'a -> (unit -> 'b t) -> 'b t
    (** Restore the previous binding after success, exception, or native
        cancellation. *)

    val protect : context -> finally:(unit -> unit) -> (unit -> 'a t) -> 'a t
    (** Run [finally] exactly once and preserve the callback's original result,
        exception, or native cancellation. *)

    val is_control_exception : context -> exn -> bool
    (** Identify native cancellation and other control-flow exceptions that the
        core must preserve rather than contain. *)
  end

  type init_error = Already_initialized | Runtime_already_registered
  type capture_error = Runtime_already_registered | Invalid_capacity of int

  exception Init_error of init_error

  module Make (Runtime : S) (Platform : Platform.S) : sig
    type +'a io = 'a Runtime.t
    type t

    val create : runtime_context:Runtime.context -> platform:Platform.t -> t
    (** Create an inert composition. This does not register or initialize the
        process route. *)

    val init : t -> Config.t -> (unit, init_error) result

    val init_exn : t -> Config.t -> unit
    (** [init_exn] raises [Init_error error] when [init] returns [Error error].
    *)

    val with_capture :
      t ->
      Config.t ->
      ?capacity:int ->
      (Capture.t -> 'a io) ->
      ('a, capture_error) result io
    (** Run with capture as the innermost dynamic route.

        Registration and capacity errors occur before [callback]. The capture
        suppresses production delivery for the dynamic extent. Prior bindings
        are restored and the capture is closed exactly once after callback
        success, exception, or cancellation. The callback outcome is preserved;
        a retained capture remains readable but closed to further delivery. *)
  end
end
