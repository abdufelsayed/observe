(** Immutable untyped structured values. *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Object of (string * t) list
  | Embedded : 'a Type.t * 'a -> t

type frozen = Snapshot.t

type integer =
  [ `Int of int | `Int32 of int32 | `Int64 of int64 | `Decimal of string ]

type truncation =
  | Depth
  | Object_fields
  | Collection
  | String_bytes
  | Bytes_length
  | Nodes
  | Total_bytes

type frozen_view =
  [ `Null
  | `Bool of bool
  | `Integer of integer
  | `Float of float
  | `String of string
  | `Bytes of string
  | `Truncated of truncation
  | `Truncated_list of frozen list * truncation
  | `Truncated_object of (string * frozen) list * truncation
  | `List of frozen list
  | `Object of (string * frozen) list
  | `Variant of string * bool * frozen option ]

val find : string list -> frozen -> frozen option
(** Find a nested object field in already completed meaning. An empty path
    returns the supplied root. Truncated objects expose only their retained safe
    prefix; missing, removed, and unretained fields return [None]. *)

val null : t
val bool : bool -> t
val int : int -> t
val float : float -> t
val string : string -> t
val option : t option -> t
val list : t list -> t
val object_ : (string * t) list -> t

val embed : 'a Type.t -> 'a -> t
(** Retain a typed value and its description without projecting it. *)

val pp : Format.formatter -> t -> unit
val to_string : t -> string

type json_error = Invalid_utf8 | Non_finite_float | Unsupported_value

val append_json : Buffer.t -> t -> (unit, json_error) result
(** Append one compact JSON value. Transactional: [Error] restores the buffer to
    its length at entry. *)

val append_pretty : Pretty.t -> Pretty.placement -> t -> unit

val freeze :
  ?limits:Log_limits.t -> t -> (Snapshot.fragment, Snapshot.error) result

val freeze_into :
  t -> Snapshot.context -> depth:int -> (Snapshot.value, Snapshot.error) result

val append_frozen_json : Buffer.t -> frozen -> unit
val append_frozen_pretty : Pretty.t -> Pretty.placement -> frozen -> unit
val view : frozen -> frozen_view
val frozen_to_json_string : frozen -> string

val to_json_string : t -> (string, json_error) result
(** Project one JSON value without repairing invalid strings or non-finite
    primitive floats. Embedded values retain the JSON semantics of their
    supplied representation; unsupported Repr projections return
    [Unsupported_value]. *)
