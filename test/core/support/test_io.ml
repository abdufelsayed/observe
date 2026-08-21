module Direct = struct
  type +'a t = 'a
  type context = unit
  type 'a key = { mutable value : 'a option }

  exception Control

  let return value = value
  let bind value callback = callback value
  let create_key () = { value = None }
  let get () key = key.value

  let with_binding () key value callback =
    let previous = key.value in
    key.value <- Some value;
    Fun.protect ~finally:(fun () -> key.value <- previous) callback

  let protect () ~finally callback = Fun.protect ~finally callback
  let is_control_exception () = function Control -> true | _ -> false
end

module Inherited = struct
  type +'a t = 'a
  type binding = int * Obj.t

  type context = {
    mutable current : context;
    mutable bindings : binding list;
    mutable protect_finally_calls : int;
  }

  type 'a key = { id : int }

  exception Cancelled

  let next_key = ref 0

  let create_context () =
    let rec context =
      { current = context; bindings = []; protect_finally_calls = 0 }
    in
    context

  let inherited_context context =
    let bindings = context.current.bindings in
    let rec child = { current = child; bindings; protect_finally_calls = 0 } in
    child

  let with_context context child callback =
    let previous = context.current in
    context.current <- child;
    Fun.protect ~finally:(fun () -> context.current <- previous) callback

  let finally_calls context = context.protect_finally_calls
  let return value = value
  let bind value callback = callback value

  let create_key () =
    let id = !next_key in
    incr next_key;
    { id }

  let get context key =
    match List.assoc_opt key.id context.current.bindings with
    | None -> None
    | Some value -> Some (Obj.obj value)

  let with_binding context key value callback =
    let previous_context = context.current in
    let previous_bindings = previous_context.bindings in
    previous_context.bindings <-
      (key.id, Obj.repr value) :: List.remove_assoc key.id previous_bindings;
    Fun.protect
      ~finally:(fun () ->
        previous_context.bindings <- previous_bindings;
        context.current <- previous_context)
      callback

  let protect context ~finally callback =
    Fun.protect
      ~finally:(fun () ->
        context.protect_finally_calls <- context.protect_finally_calls + 1;
        finally ())
      callback

  let is_control_exception _ = function Cancelled -> true | _ -> false
end

module Host = struct
  type t = {
    console_style : Observe.Formatter.style;
    now : unit -> (Observe.Timestamp.t, Observe.IO.clock_error) result;
    monotonic_now : unit -> (int64, Observe.IO.clock_error) result;
    next_id : unit -> (string, Observe.IO.clock_error) result;
    offer_console : string -> Observe.IO.console_acceptance;
  }

  let create ?(console_style = Observe.Formatter.Plain)
      ?(now = fun () -> Ok (Observe.Timestamp.of_unix_ns 42L)) ?monotonic_now
      ?next_id ?(offer_console = fun _ -> Observe.IO.Accepted) () =
    let monotonic_now =
      match monotonic_now with
      | Some monotonic_now -> monotonic_now
      | None ->
          let next = ref 100L in
          fun () ->
            let value = !next in
            next := Int64.add value 25L;
            Ok value
    in
    let next_id =
      match next_id with
      | Some next_id -> next_id
      | None ->
          let next = ref 0 in
          fun () ->
            incr next;
            Ok ("operation-" ^ string_of_int !next)
    in
    { console_style; now; monotonic_now; next_id; offer_console }

  let console_style t = t.console_style
  let now t = t.now ()
  let monotonic_now t = t.monotonic_now ()
  let next_id t = t.next_id ()
  let offer_console t output = t.offer_console output
end

module IO = struct
  type +'a t = 'a
  type state = Host.t
  type 'a key = 'a Direct.key

  let return = Direct.return
  let bind = Direct.bind
  let create_key = Direct.create_key
  let get _state key = Direct.get () key

  let with_binding _state key value callback =
    Direct.with_binding () key value callback

  let protect _state ~finally callback = Direct.protect () ~finally callback
  let is_control_exception _state exn = Direct.is_control_exception () exn

  module Clock = struct
    let now = Host.now
    let monotonic_now = Host.monotonic_now
  end

  module Identity = struct
    let next = Host.next_id
  end

  module Console = struct
    let style = Host.console_style
    let offer = Host.offer_console
  end
end

module Inherited_io = struct
  type +'a t = 'a
  type state = { context : Inherited.context; host : Host.t }
  type 'a key = 'a Inherited.key

  let create ~context ~host = { context; host }
  let return = Inherited.return
  let bind = Inherited.bind
  let create_key = Inherited.create_key
  let get state key = Inherited.get state.context key

  let with_binding state key value callback =
    Inherited.with_binding state.context key value callback

  let protect state ~finally callback =
    Inherited.protect state.context ~finally callback

  let is_control_exception state exn =
    Inherited.is_control_exception state.context exn

  module Clock = struct
    let now state = Host.now state.host
    let monotonic_now state = Host.monotonic_now state.host
  end

  module Identity = struct
    let next state = Host.next_id state.host
  end

  module Console = struct
    let style state = Host.console_style state.host
    let offer state output = Host.offer_console state.host output
  end
end

let config ?environment ?version ?enabled ?console ?min_level ?drains service =
  Observe.Config.create_exn ~service ?environment ?version ?enabled ?console
    ?min_level ?drains ()

let diagnostic_count entries kind =
  List.fold_left
    (fun total (entry : Observe.Diagnostics.entry) ->
      if entry.kind = kind then total + entry.count else total)
    0 entries

let process_diagnostic_count kind =
  diagnostic_count (Observe.Diagnostics.snapshot ()) kind

let text_payload log =
  match Observe.Log.body log with
  | Observe.Log.Text { tag; message } -> Some (tag, message)
  | Observe.Log.Structured _ -> None

let text ~tag message (builder : Observe.Logs.builder) =
  builder.text ~tag "%s" message

let untyped value (builder : Observe.Logs.builder) = builder.value value

let typed description value (builder : Observe.Logs.builder) =
  builder.typed description value
