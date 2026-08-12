type t

val create :
  ?style:Observe.Formatter.style ->
  ?clock:(unit -> (Observe.Instant.t, Observe.IO.clock_error) result) ->
  ?console:(string -> Observe.IO.console_acceptance) ->
  unit ->
  t

module IO : Observe.IO.S with type 'a t = 'a and type state = t
