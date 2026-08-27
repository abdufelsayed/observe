type t

val create :
  ?style:Observe.Formatter.style ->
  ?clock:(unit -> (Observe.Timestamp.t, Observe.IO.clock_error) result) ->
  ?monotonic_now:(unit -> (int64, Observe.IO.clock_error) result) ->
  ?next_id:(unit -> (string, Observe.IO.clock_error) result) ->
  ?sampling_draw:(unit -> float) ->
  ?console:(string -> Observe.IO.console_acceptance) ->
  unit ->
  t

module IO : Observe.IO.S with type 'a t = 'a and type state = t
