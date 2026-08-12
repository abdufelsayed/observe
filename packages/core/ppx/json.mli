val declaration :
  (module Ppx_repr_lib.Engine.S) ->
  library:string option ->
  encoders:(string * string) list ->
  Ppxlib.type_declaration ->
  Ppxlib.expression ->
  Ppxlib.expression ->
  Ppxlib.expression
