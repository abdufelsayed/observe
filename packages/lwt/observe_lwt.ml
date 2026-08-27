type state = {
  clock : unit -> (Observe.Timestamp.t, Observe.IO.clock_error) result;
  monotonic_now : unit -> (int64, Observe.IO.clock_error) result;
  next_id : unit -> (string, Observe.IO.clock_error) result;
  sampling_draw : unit -> float;
  create_stable_sampling_draw : unit -> unit -> float;
  console_style : unit -> Observe.Formatter.style;
  offer_console : string -> Observe.IO.console_acceptance;
  can_lookup_context : unit -> bool;
}

type t = state

let create ~clock ~monotonic_now ~next_id ~sampling_draw
    ~create_stable_sampling_draw ~console_style ~offer_console
    ~can_lookup_context () =
  {
    clock;
    monotonic_now;
    next_id;
    sampling_draw;
    create_stable_sampling_draw;
    console_style;
    offer_console;
    can_lookup_context;
  }

module IO = struct
  type +'a t = 'a Lwt.t
  type nonrec state = state
  type 'a key = 'a Lwt.key

  let return = Lwt.return
  let bind = Lwt.bind

  let observe callback =
    Lwt.try_bind callback
      (fun value -> Lwt.return (Observe.IO.Returned value))
      (fun raised ->
        let backtrace = Printexc.get_raw_backtrace () in
        Lwt.return (Observe.IO.Raised (raised, backtrace)))

  let repropagate raised backtrace =
    Printexc.raise_with_backtrace raised backtrace

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
    let monotonic_now state = state.monotonic_now ()
  end

  module Identity = struct
    let next state = state.next_id ()
  end

  module Sampling = struct
    type stable = unit -> float

    let draw state = state.sampling_draw ()
    let create_stable state = state.create_stable_sampling_draw ()
    let draw_stable _state stable = stable ()
  end

  module Console = struct
    let style state = state.console_style ()
    let offer state output = state.offer_console output
  end
end
