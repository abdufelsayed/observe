type state = {
  clock : unit -> (Observe.Timestamp.t, Observe.IO.clock_error) result;
  console_style : unit -> Observe.Formatter.style;
  offer_console : string -> Observe.IO.console_acceptance;
  can_lookup_context : unit -> bool;
}

type t = state

let create ~clock ~console_style ~offer_console ~can_lookup_context () =
  { clock; console_style; offer_console; can_lookup_context }

module IO = struct
  type +'a t = 'a Lwt.t
  type nonrec state = state
  type 'a key = 'a Lwt.key

  let return = Lwt.return
  let bind = Lwt.bind
  let create_key = Lwt.new_key
  let get state key = if state.can_lookup_context () then Lwt.get key else None

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
    let offer state output = state.offer_console output
  end
end
