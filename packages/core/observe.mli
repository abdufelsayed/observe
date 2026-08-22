(** Portable structured logging.

    The core owns logging policy and performs no I/O. Integrations provide one
    completed {!module-type:IO.S} implementation. Ordinary caller code logs
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

module Schema : sig
  type ('record, 'builder) t
  type 'record patch
end

module Error : sig
  type roles
  type 'error t

  val roles :
    ?kind:string ->
    ?code:string ->
    ?message:string ->
    ?explanation:string ->
    ?remediation:string ->
    ?documentation:string ->
    unit ->
    roles

  val create : ('error -> roles) -> 'error t
  (** Build a reusable interpretation without replacing the consumer's domain
      error type. *)

  val exn : exn t
  (** Interpret an explicitly supplied exception. Pass its captured raw
      backtrace at the contribution site when one is available. *)
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

  type frozen
  (** Immutable package-owned structured meaning in a completed observation. *)

  val frozen_to_json_string : frozen -> string

  val to_json_string : t -> (string, json_error) result
  (** Encode one compact JSON value with Observe's direct writer. *)
end

module Generated_runtime : sig
  (** Compatibility contract for code emitted by [observe.ppx]. Consumer code
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

  type freeze_context
  type frozen
  type freeze_error

  val with_freeze :
    'a description ->
    (freeze_context -> depth:int -> 'a -> (frozen, freeze_error) result) ->
    'a description

  val with_recursive_plan :
    'a description -> ('a description -> 'a -> rendered) -> 'a description

  val with_recursive_freeze :
    'a description ->
    ('a description ->
    freeze_context ->
    depth:int ->
    'a ->
    (frozen, freeze_error) result) ->
    'a description

  val freeze :
    'a description ->
    freeze_context ->
    depth:int ->
    'a ->
    (frozen, freeze_error) result

  val frozen_string :
    freeze_context -> depth:int -> string -> (frozen, freeze_error) result

  val frozen_object :
    freeze_context ->
    depth:int ->
    (string * frozen) list ->
    (frozen, freeze_error) result

  val frozen_variant :
    freeze_context ->
    depth:int ->
    bool ->
    string ->
    (frozen, freeze_error) result

  val frozen_variant_payload :
    freeze_context ->
    depth:int ->
    bool ->
    string ->
    frozen ->
    (frozen, freeze_error) result

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

  type fragment
  type patch_field
  type untyped_patch

  val fragment : 'a description -> 'a -> fragment

  val error_fragment :
    'error Error.t -> ?backtrace:Printexc.raw_backtrace -> 'error -> fragment

  val patch_fragment : 'record Schema.patch -> fragment
  val patch_field : string -> fragment -> patch_field

  val record_patch :
    ('record, 'builder) Schema.t ->
    patch_field option list ->
    'record Schema.patch

  val record_patch_fields :
    ('record, 'builder) Schema.t -> patch_field list -> 'record Schema.patch

  val named_record_patch :
    string -> patch_field option list -> 'record Schema.patch

  val named_record_patch_fields :
    string -> patch_field list -> 'record Schema.patch

  val named_error_patch : string -> fragment -> 'record Schema.patch

  val combine_named_patches :
    string -> 'record Schema.patch list -> 'record Schema.patch

  val record_schema :
    ?name:string ->
    builder:(string -> 'builder) ->
    'record description ->
    ('record, 'builder) Schema.t

  val schema_builder : ('record, 'builder) Schema.t -> 'builder

  val untyped_value_patch : Value.t -> untyped_patch
  (** PPX runtime bridge for an annotation-free anonymous object. *)
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

module Log : sig
  type t
  (** A completed admitted log whose structured body is an immutable bounded
      package-owned snapshot. *)

  type body =
    | Text of { tag : string; message : string }
    | Structured of { origin : structured_origin; value : Value.frozen }

  and structured_origin = Open | Declared of string

  type kind = Point | Wide
  type operation

  val service : t -> string
  val environment : t -> string option
  val version : t -> string option
  val timestamp : t -> Timestamp.t
  val level : t -> Level.t
  val body : t -> body

  val kind : t -> kind
  (** Distinguish an auto-emitted point observation from a completed wide
      operation without inspecting presentation output. *)

  val operation : t -> operation option
  (** The immutable operation envelope on a wide observation. Point observations
      return [None], including correlated points. *)

  val correlation_id : t -> string option
  (** The associated wide-log occurrence identifier on a point log. *)

  val operation_name : operation -> string
  val operation_id : operation -> string
  val operation_parent_id : operation -> string option

  val operation_duration_ns : operation -> int64
  (** The non-negative monotonic elapsed duration. *)
end

module Diagnostics : sig
  type kind =
    | Not_initialized
    | No_delivery_target
    | Capture_lookup_raised
    | Operation_lookup_raised
    | Clock_unavailable
    | Clock_raised
    | Identity_unavailable
    | Identity_raised
    | Monotonic_clock_unavailable
    | Monotonic_clock_raised
    | Message_evaluation_raised
    | Canonical_freeze_failed
    | Post_seal_set
    | Post_seal_set_level
    | Post_seal_emit
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
  (** Construct an additional output with a synchronous callback over one
      immutable completed observation. A drain can retain [Log.t] safely; it
      must still own destination-specific projection and mutable delivery state.
      [Accepted] means immediate ownership acceptance only. Ordinary callback
      exceptions are contained as [Diagnostics.Drain_raised]; runtime control
      exceptions are preserved. *)

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
      millisecond timestamps and console-safe caller data. Wide headers show
      operation identity, human-readable duration, and optional parent identity.
  *)

  val json : t
  (** Render one compact object. Correlated points add [operation_id]. Wide logs
      add a nested [operation] object and keep consumer data under [body]. Exact
      timestamps and durations are decimal nanosecond strings. *)

  val ndjson : t
  (** The same compact object as {!json}, followed by one line feed. *)
end

module Capture : sig
  type t

  val default_capacity : int

  val logs : t -> Log.t list
  (** Immutable completed observations retained in admission order. *)

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
      ]}

      With [observe.ppx], the exact call above can be written as:

      {[
      [%observe.info text ~tag:"auth" "user %d logged in" user_id]
      ]} *)

  type message
  (** A completed authoring result produced inside an admitted callback. *)

  type object_
  type field
  type untyped_patch = Generated_runtime.untyped_patch

  type untyped_builder = private {
    untyped : object_;
    field : 'a. string -> 'a Type.t -> 'a -> field;
    object_ : string -> (untyped_builder -> untyped_patch) -> field;
    error :
      'error.
      'error Error.t ->
      ?backtrace:Printexc.raw_backtrace ->
      'error ->
      untyped_patch;
    seal : object_ -> untyped_patch;
  }

  type builder = private {
    text :
      'a. tag:string -> ('a, Format.formatter, unit, message) format4 -> 'a;
    untyped : object_;
    field : 'a. string -> 'a Type.t -> 'a -> field;
    object_ : string -> (untyped_builder -> untyped_patch) -> field;
    seal : object_ -> message;
    value : Value.t -> message;
    error :
      'error.
      'error Error.t -> ?backtrace:Printexc.raw_backtrace -> 'error -> message;
    typed : 'a 'builder. ('a, 'builder) Schema.t -> 'a -> message;
  }
  (** The admitted point builder. [text] supports type-safe format strings;
      [untyped], [field], [object_], and [seal] author anonymous record-shaped
      structure; [value] is the explicit {!Value} compatibility path; and
      [typed] accepts a complete value through a record schema. Every accepted
      result is frozen before publication. *)

  type author = builder -> message
  (** Message authoring invoked only after route and level admission. *)

  type ('builder, 'patch) t

  val log :
    ?operation:('operation_builder, 'operation_patch) t ->
    level:Level.t ->
    author ->
    unit
  (** Emit through the active scoped or production route. Before installation,
      messages are withheld and diagnosed. Logging does not raise merely because
      process initialization has not happened. Ordinary authoring exceptions are
      withheld and diagnosed as [Diagnostics.Message_evaluation_raised].
      [operation] explicitly associates the separate point observation with that
      wide-log occurrence. *)

  val debug : ?operation:('builder, 'patch) t -> author -> unit
  val info : ?operation:('builder, 'patch) t -> author -> unit
  val warn : ?operation:('builder, 'patch) t -> author -> unit
  val error : ?operation:('builder, 'patch) t -> author -> unit
  val ( |+ ) : object_ -> field -> object_

  val create :
    ?parent:('parent_builder, 'parent_patch) t ->
    name:string ->
    unit ->
    (untyped_builder, untyped_patch) t
  (** Start an empty untyped wide log at [Info]. An unavailable route or
      required runtime capability produces an inert handle. *)

  val create_typed :
    ?parent:('parent_builder, 'parent_patch) t ->
    name:string ->
    ('record, 'builder) Schema.t ->
    ('builder, 'record Schema.patch) t
  (** Start an empty wide log locked to one declared record schema. Its
      contributions are sparse patches; no field is mandatory at emission. *)

  val set : ('builder, 'patch) t -> ('builder -> 'patch) -> unit
  (** Lazily contribute one record-shaped patch while the handle is active.
      Objects merge recursively and later non-object values replace earlier
      values. Failed contributions seal and withhold the lifecycle. *)

  val set_level : ('builder, 'patch) t -> Level.t -> unit
  (** Replace the explicit level. The last explicit value wins over derived
      [Error] regardless of call order. *)

  val emit : ('builder, 'patch) t -> unit
  (** Seal and attempt final-level admission and publication exactly once. *)

  module Terminal : sig
    type ('builder, 'patch) log = ('builder, 'patch) t
    type ('builder, 'patch) t

    val create :
      error:exn Error.t -> ('builder, 'patch) log -> ('builder, 'patch) t
    (** Create a single-use terminal owner for an existing wide log. *)

    val complete :
      ('builder, 'patch) t -> ?set:('builder -> 'patch) -> unit -> unit

    val fail :
      ('builder, 'patch) t ->
      ?set:('builder -> 'patch) ->
      ?backtrace:Printexc.raw_backtrace ->
      exn ->
      unit

    val cancel :
      ('builder, 'patch) t -> ?set:('builder -> 'patch) -> unit -> unit
    (** The first terminal action wins and emits the same ordinary lifecycle.
        Its optional [set] contribution is authored only by that winner and
        before emission. [fail] then contributes the selected safe error
        interpretation; cancellation contributes no inferred fields or level. *)
  end
end

module IO : sig
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
    (** Observe effect settlement as the exact result or the same exception and
        raw backtrace at the runtime failure boundary. Cancellation remains a
        classified [Raised] outcome. *)

    val repropagate : exn -> Printexc.raw_backtrace -> 'a t
    (** Re-propagate that exception through the runtime effect. *)

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

      val monotonic_now : state -> (int64, clock_error) result
      (** Return a process-relative monotonic nanosecond value. *)
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

  val with_wide : t -> ('builder, 'patch) Logs.t -> (unit -> 'a io) -> 'a io
  (** Bind an existing wide log for scoped point-log correlation only. This
      catches nothing and emits nothing. *)

  val manage :
    t ->
    ('builder, 'patch) Logs.t ->
    error:exn Error.t ->
    (unit -> 'a io) ->
    'a io
  (** Run work inside the wide-log scope, contribute an escaping ordinary
      exception through [error], emit once, and preserve the original runtime
      outcome. Native cancellation completes without inferred error meaning. *)

  val fork :
    t ->
    parent:('parent_builder, 'parent_patch) Logs.t ->
    name:string ->
    error:exn Error.t ->
    ((Logs.untyped_builder, Logs.untyped_patch) Logs.t -> 'a io) ->
    'a io

  val fork_typed :
    t ->
    parent:('parent_builder, 'parent_patch) Logs.t ->
    name:string ->
    ('record, 'builder) Schema.t ->
    error:exn Error.t ->
    (('builder, 'record Schema.patch) Logs.t -> 'a io) ->
    'a io
end
