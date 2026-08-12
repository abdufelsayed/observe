type state = {
  clock : unit -> (Observe.Instant.t, Observe.IO.clock_error) result;
  console_style : unit -> Observe.Formatter.style;
  write_console : string -> Observe.IO.console_acceptance;
}

type t = state

let create ~clock ~console_style ~write_console () =
  { clock; console_style; write_console }

module IO = struct
  type +'a t = 'a Lwt.t
  type nonrec state = state
  type 'a key = 'a Lwt.key

  let return = Lwt.return
  let bind = Lwt.bind
  let create_key = Lwt.new_key
  let get _state key = Lwt.get key

  let with_binding _state key value callback =
    Lwt.with_value key (Some value) callback

  let protect _state ~finally callback =
    Lwt.finalize callback (fun () ->
        finally ();
        Lwt.return_unit)

  let is_control_exception _state = function Lwt.Canceled -> true | _ -> false

  module Clock = struct
    let now state = state.clock ()
  end

  module Console = struct
    let style state = state.console_style ()
    let write state output = state.write_console output
  end
end
