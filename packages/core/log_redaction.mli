(** Caller-defined disclosure policy for completed logging values. This module
    is kept private at the package level and is re-exported by [Logs] with the
    ordinary public logging hierarchy. *)

module Path : sig
  type t

  val root : t
  val fields : string list -> t
  val field : string -> t -> t
  val index : int -> t -> t
  val case : string -> t -> t
  val to_string : t -> string
end

module Matcher : sig
  type t

  val string_equal : string -> t
  val string_prefix : string -> t
  val string_suffix : string -> t
  val string_contains : string -> t
  val bool : bool -> t
  val int : int -> t
  val int32 : int32 -> t
  val int64 : int64 -> t
  val float : float -> t
  val bytes_equal : bytes -> t
  val null : t
end

module Mask : sig
  type hidden = Fill of string | Collapse of string
  type t

  val keep_prefix : characters:int -> hidden:hidden -> unit -> t
  val keep_suffix : characters:int -> hidden:hidden -> unit -> t
  val keep_ends : characters:int -> hidden:hidden -> unit -> t
  val custom : ?fallback:string -> (string -> string) -> t
end

module Action : sig
  type t

  val remove : t
  val replace : Value.t -> t
  val mask : Mask.t -> t
end

module Rule : sig
  type t

  val at : Path.t -> Action.t -> t
  val matching : Matcher.t -> Action.t -> t
end

type t
type error

val create :
  ?using:('record, 'builder) Schema.t ->
  rules:Rule.t list ->
  unit ->
  (t, error) result

val create_exn :
  ?using:('record, 'builder) Schema.t -> rules:Rule.t list -> unit -> t

val combine : policies:t list -> unit -> (t, error) result
val combine_exn : policies:t list -> unit -> t
val none : t
val is_none : t -> bool
val pp_error : Format.formatter -> error -> unit

exception Invalid_redaction of error

(** Private implementation seam. [Observe] exposes the constructors above, while
    the engine consumes these package-owned compiled values. *)

module Internal : sig
  type compiled_pair

  val exact : compiled_pair -> Snapshot.Redaction.compiled_exact
  val matching : compiled_pair -> Snapshot.Redaction.compiled_matching

  val compiled : t -> schema:Schema.identity option -> compiled_pair
  (** Return the already-compiled rules applicable to [schema]. The lookup
      performs no value traversal, callback invocation, or rule conversion, so
      it is safe to use on every completed observation. *)

  val is_none : t -> bool
end
