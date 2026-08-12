type 'a t

val repr : 'a t -> 'a Repr.t

val plan : 'a t -> 'a -> Pretty.rendered
(** Classify and project a value once, returning the rendering step that owns
    the value's placement. *)

val pretty : 'a t -> Pretty.t -> Pretty.placement -> 'a -> unit

val append_json : Buffer.t -> 'a t -> 'a -> unit
(** Append one compact JSON value. Transactional: an exception restores the
    buffer to its length at entry and propagates unchanged. *)

val of_repr : ?json:(Buffer.t -> 'a -> unit) -> 'a Repr.t -> 'a t
(** Lift an opaque Repr description through the compatibility path. A supplied
    writer must append one compact value accepted by the Repr decoder. Pretty
    formatting still projects through Repr because an opaque description does
    not expose the structure needed by Observe's direct renderer. Fully
    described combinators and generated descriptions use direct writers. *)

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
(** A sequence is enumerated exactly once per projection. The pretty plan
    inspects the first cell to choose a layout and continues from that cell;
    effectful sequences are neither restarted nor enumerated twice. *)

val ref : 'a t -> 'a ref t
val lazy_t : 'a t -> 'a Lazy.t t
val queue : 'a t -> 'a Queue.t t
val stack : 'a t -> 'a Stack.t t
val hashtbl : 'k t -> 'v t -> ('k, 'v) Hashtbl.t t

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
  ?polymorphic:bool -> ?is:('a -> bool) -> string -> 'a -> ('a, 'a case_p) case

val case1 :
  ?polymorphic:bool ->
  ?project:('a -> 'b option) ->
  string ->
  'b t ->
  ('b -> 'a) ->
  ('a, 'b -> 'a case_p) case

val ( |~ ) :
  ('a, 'b, 'c -> 'd) open_variant -> ('a, 'c) case -> ('a, 'b, 'd) open_variant

val sealv : ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t
(** Seal a variant. Selectors are all-or-nothing: when every case supplies
    [~is]/[~project], the description uses direct JSON and pretty writers and
    each selector runs at most once per projection; when no case does, the
    description deliberately uses the Repr compatibility projection. Mixing
    cases with and without selectors raises [Invalid_argument]. *)

val enum : string -> (string * 'a) list -> 'a t

val mu : ('a t -> 'a t) -> 'a t
(** The builder runs at construction and again whenever Repr stages an unrolling
    generic, so it must be pure and must not force the recursive description
    during construction. The first built writers remain the description's
    writers. *)

val mu2 : ('a t -> 'b t -> 'a t * 'b t) -> 'a t * 'b t
(** Same builder contract as {!mu}. *)

type +'a staged = 'a Repr.staged

val stage : 'a -> 'a staged
val unstage : 'a staged -> 'a

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

val to_json_string : 'a t -> 'a -> string
(** Allocate one compact JSON value using the attached writer. *)

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

val map : 'b t -> ('b -> 'a) -> ('a -> 'b) -> 'a t

module Generated_runtime : sig
  (** Runtime contract for code generated by [observe.ppx]. This surface is
      stable for generated code only; application code uses the ordinary
      combinators. *)

  type renderer
  type placement

  type rendered =
    | Scalar of (renderer -> unit)
    | Node of (renderer -> placement -> unit)

  val with_json : 'a t -> (Buffer.t -> 'a -> unit) -> 'a t
  val with_plan : 'a t -> ('a -> rendered) -> 'a t
  val with_recursive_plan : 'a t -> ('a t -> 'a -> rendered) -> 'a t
  val json : 'a t -> Buffer.t -> 'a -> unit
  val plan : 'a t -> 'a -> rendered
  val is_scalar : 'a t -> 'a -> bool
  val render : 'a t -> renderer -> placement -> 'a -> unit
  val inline : placement
  val start : renderer -> placement -> scalar:bool -> bool
  val finish : renderer -> bool -> unit
  val field : 'a t -> renderer -> last:bool -> name:string -> 'a -> unit
  val constructor : 'a t -> renderer -> last:bool -> name:string -> 'a -> unit

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
  val json_field : 'a t -> Buffer.t -> bool -> string -> 'a -> bool
end
