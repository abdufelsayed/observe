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
    ?polymorphic:bool -> is:('a -> bool) -> string -> 'a -> ('a, 'a case_p) case

  val case1 :
    ?polymorphic:bool ->
    project:('a -> 'b option) ->
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
  type 'record patch_builder

  val record :
    ?name:string ->
    builder:('record patch_builder -> 'builder) ->
    'record Type.t ->
    ('record, 'builder) t
  (** Create a record-root schema and its typed sparse-patch builder. The
      callback receives an opaque capability tied to this schema instance. *)

  val field :
    'record patch_builder -> string -> 'a Type.t -> 'a -> 'record patch
  (** Build one typed field contribution. *)

  val nested :
    'record patch_builder ->
    string ->
    using:('nested, 'nested_builder) t ->
    'nested patch ->
    'record patch
  (** Build one nested record contribution after checking its schema. *)

  val combine : 'record patch_builder -> 'record patch list -> 'record patch
  val name : ('record, 'builder) t -> string
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

  type integer =
    [ `Int of int | `Int32 of int32 | `Int64 of int64 | `Decimal of string ]

  type frozen_view =
    [ `Null
    | `Bool of bool
    | `Integer of integer
    | `Float of float
    | `String of string
    | `Bytes of string
    | `List of frozen list
    | `Object of (string * frozen) list
    | `Variant of string * bool * frozen option ]

  val view : frozen -> frozen_view
  (** Inspect already completed structured meaning without parsing JSON or
      receiving mutable snapshot internals. Children remain [frozen] and are
      traversed recursively through [view]. *)

  val frozen_to_json_string : frozen -> string

  val to_json_string : t -> (string, json_error) result
  (** Encode one compact JSON value with Observe's direct writer. *)
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
  (** A completed admitted log whose structured event is an immutable bounded
      package-owned snapshot. *)

  type event =
    | Text of { tag : string; message : string }
    | Structured of { origin : structured_origin; value : Value.frozen }

  and structured_origin = Open | Declared of string

  type operation_reference
  type operation
  type annotation

  type kind =
    | Point of { correlation : operation_reference option }
    | Wide of { operation : operation; annotations : annotation list }

  val service : t -> string
  val environment : t -> string option
  val version : t -> string option
  val timestamp : t -> Timestamp.t
  val level : t -> Level.t
  val event : t -> event

  val kind : t -> kind
  (** Complete point or wide meaning. Correlation exists only on point logs;
      operation facts and annotations exist only on wide logs. *)

  val operation_reference_name : operation_reference -> string
  val operation_reference_id : operation_reference -> string
  val operation_name : operation -> string
  val operation_id : operation -> string
  val operation_parent : operation -> operation_reference option

  val operation_duration_ns : operation -> int64
  (** The non-negative monotonic elapsed duration. *)

  val annotation_timestamp : annotation -> Timestamp.t
  val annotation_level : annotation -> Level.t
  val annotation_message : annotation -> string
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
    | Post_seal_annotate
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
    | Runtime_closed

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
  (** Render one flat compact event. Package metadata uses documented reserved
      root fields, while consumer structured fields remain at the root.
      Timestamps are RFC 3339 UTC strings with nanosecond precision and wide
      durations are numeric milliseconds. *)

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
  type untyped_patch

  type untyped_builder = private {
    untyped : object_;
    field : 'a. string -> 'a Type.t -> 'a -> field;
    object_ : string -> (untyped_builder -> untyped_patch) -> field;
    error :
      'error.
      using:'error Error.t ->
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
    error :
      'error.
      using:'error Error.t ->
      ?backtrace:Printexc.raw_backtrace ->
      'error ->
      message;
    typed : 'a 'builder. using:('a, 'builder) Schema.t -> 'a -> message;
  }
  (** The admitted point builder. [text] supports type-safe format strings;
      [untyped], [field], [object_], and [seal] author anonymous record-shaped
      structure; [typed] accepts a complete value through a record schema. Every
      accepted result is frozen before publication. *)

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
    using:('record, 'builder) Schema.t ->
    unit ->
    ('builder, 'record Schema.patch) t
  (** Start an empty wide log locked to one declared record schema. Its
      contributions are sparse patches; no field is mandatory at emission. *)

  val set : ('builder, 'patch) t -> ('builder -> 'patch) -> unit
  (** Lazily contribute one record-shaped patch while the handle is active.
      Objects merge recursively and later non-object values replace earlier
      values. Failed contributions seal and withhold the lifecycle. *)

  val set_level : ('builder, 'patch) t -> level:Level.t -> unit
  (** Replace the explicit level. The last explicit value wins over derived
      [Error] regardless of call order. *)

  val annotate :
    ('builder, 'patch) t -> level:Level.t -> (unit -> string) -> unit
  (** Append one explicit timestamped entry to the completed wide operation. The
      callback remains lazy, and separate point logs are never copied into the
      operation's annotation list. *)

  val emit : ('builder, 'patch) t -> unit
  (** Seal and attempt final-level admission and publication exactly once. With
      no in-flight author, completion happens before [emit] returns. If another
      thread is already evaluating an admitted author, [emit] seals and returns;
      the last admitted author performs completion. *)

  type current_error =
    | Not_bound
    | Expected_open
    | Expected_typed
    | Schema_mismatch

  exception Current_error of current_error

  val current : unit -> (untyped_builder, untyped_patch) t
  (** Return the open wide log bound by the innermost operation scope. *)

  val current_typed :
    using:('record, 'builder) Schema.t -> ('builder, 'record Schema.patch) t
  (** Return the schema-locked wide log bound by the innermost operation scope.
      The supplied schema must be the same instance used to start it. *)
end

module Ppx_runtime : sig
  (** Generated-code contract for [observe.ppx]. Ordinary code uses {!Type},
      {!Schema}, and {!Logs}; this hierarchy changes only with a coordinated
      PPX/runtime change. *)

  type 'a type_description = 'a Type.t
  type logs_message = Logs.message
  type logs_untyped_patch = Logs.untyped_patch

  module Type : sig
    type 'a description = 'a type_description
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
  end

  module Schema : sig
    type fragment
    type patch_field

    val fragment : 'a type_description -> 'a -> fragment

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

    val identified_record_patch :
      'record Schema.patch_builder ->
      patch_field option list ->
      'record Schema.patch

    val identified_record_patch_fields :
      'record Schema.patch_builder -> patch_field list -> 'record Schema.patch

    val identified_error_patch :
      'record Schema.patch_builder -> fragment -> 'record Schema.patch

    val combine_identified_patches :
      'record Schema.patch_builder ->
      'record Schema.patch list ->
      'record Schema.patch

    val record_schema :
      ?name:string ->
      builder:('record Schema.patch_builder -> 'builder) ->
      'record type_description ->
      ('record, 'builder) Schema.t

    val schema_builder : ('record, 'builder) Schema.t -> 'builder
  end

  module Logs : sig
    type message = logs_message
    type untyped_patch = logs_untyped_patch

    val untyped_value_patch : Value.t -> untyped_patch
    val untyped_message : Value.t -> message

    val is_reserved_field : string -> bool
    (** Early generated-code diagnostic backed by the runtime's authoritative
        envelope ownership predicate. *)
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

type init_error = Already_initialized | IO_already_registered | Runtime_closed

type capture_error =
  | IO_already_registered
  | Invalid_capacity of int
  | Runtime_closed

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

  val close : t -> unit
  (** Stop production admission for this process-wide runtime. This performs no
      flushing or I/O. Repeated calls are harmless. *)

  val with_capture :
    t ->
    config:Config.t ->
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

  val with_operation :
    t ->
    name:string ->
    ?using:('record, 'builder) Schema.t ->
    ?error:exn Error.t ->
    (unit -> 'a io) ->
    'a io
  (** Create one wide log, bind it as current while the callback runs, and make
      one final publication attempt when the callback settles. Ordinary escaping
      exceptions are contributed with [error], which defaults to {!Error.exn},
      then re-propagated with their original backtrace. Failed error
      interpretation seals and withholds the invalid observation. Runtime
      control exceptions complete the operation without inferred error meaning.
  *)

  val fork :
    t ->
    name:string ->
    ?using:('record, 'builder) Schema.t ->
    ?error:exn Error.t ->
    (unit -> 'a io) ->
    'a io
  (** Run an independently completed child of the current operation. The child
      records the parent's complete reference without copying or modifying its
      event. The prior current operation is restored when the callback settles.
      Raises [Logs.Current_error Not_bound] outside an operation scope. *)
end
