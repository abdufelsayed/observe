open Ppxlib

type context

val create_context : ?recursive:(string * expression) list -> unit -> context
val bindings : context -> value_binding list

val declaration :
  context ->
  (module Ppx_repr_lib.Engine.S) ->
  library:string option ->
  type_declaration ->
  expression ->
  expression ->
  expression ->
  expression

val scalar :
  context ->
  (module Ppx_repr_lib.Engine.S) ->
  library:string option ->
  type_declaration ->
  expression ->
  expression
