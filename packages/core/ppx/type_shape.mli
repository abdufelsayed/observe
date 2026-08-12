open Ppxlib

type builtin =
  | Unit
  | Bool
  | Char
  | Int
  | Int32
  | Int64
  | Float
  | String
  | Bytes
  | List
  | Array
  | Option
  | Lazy

val builtin : core_type -> builtin option
val has_custom_repr : core_type -> bool
val validate_group : rec_flag * type_declaration list -> unit

val expand_descriptor :
  (module Ppx_repr_lib.Engine.S) ->
  library:string option ->
  core_type ->
  expression
