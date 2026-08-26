(** The structural part of an [Observe.Type.t] needed by logging policy
    validation.

    This module is private to the core library. It deliberately does not inspect
    [Repr.t]: descriptions publish their logging shape while they are built, so
    policy validation never has to depend on Repr's implementation or walk a
    value. *)

type t
type step = Field of string | Index of int | Case of string
type lookup = Known of t | Empty_case | Missing | Opaque | Unaddressable

val scalar : t
val string : t
val opaque : t
val unaddressable : t
val record : (string * t) list -> t
val variant : (string * t option) list -> t
val tuple : t list -> t
val collection : t -> t
val option : t -> t

type knot

val knot : unit -> knot
val knot_shape : knot -> t
val tie : knot -> t -> unit
val lookup : t -> step list -> lookup
val accepts_string_mask : t -> bool
