type 'a binding_key = { mutable value : 'a option }

type t = {
  style : Observe.Formatter.style;
  clock : unit -> (Observe.Instant.t, Observe.IO.clock_error) result;
  console : string -> Observe.IO.console_acceptance;
}

type host = t

let create ?(style = Observe.Formatter.Plain)
    ?(clock = fun () -> Ok (Observe.Instant.of_epoch_nanoseconds 42L))
    ?(console =
      fun output ->
        ignore (Sys.opaque_identity output : string);
        Observe.IO.Accepted) () =
  { style; clock; console }

module IO = struct
  type +'a t = 'a
  type state = host
  type 'a key = 'a binding_key

  let return value = value
  let bind value callback = callback value
  let create_key () = { value = None }
  let get _state key = key.value

  let with_binding _state key value callback =
    let previous = key.value in
    key.value <- Some value;
    Fun.protect ~finally:(fun () -> key.value <- previous) callback

  let protect _state ~finally callback = Fun.protect ~finally callback
  let is_control_exception _state _exception = false

  module Clock = struct
    let now state = state.clock ()
  end

  module Console = struct
    let style state = state.style
    let write state output = state.console output
  end
end
