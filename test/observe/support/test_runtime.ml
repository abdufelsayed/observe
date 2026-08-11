module Runtime = struct
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

module Inherited_runtime = struct
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

module Platform = struct
  type t = {
    now : unit -> (Observe.Instant.t, Observe.Platform.clock_error) result;
    write_terminal : string -> Observe.Platform.terminal_acceptance;
  }

  let create ?(now = fun () -> Ok (Observe.Instant.of_epoch_nanoseconds 42L))
      ?(write_terminal = fun _ -> Observe.Platform.Accepted) () =
    { now; write_terminal }

  let now t = t.now ()
  let write_terminal t output = t.write_terminal output
end

let config ?environment ?version ?enabled ?pretty ?silent ?min_level ?drains
    service =
  Observe.Config.create_exn ~service ?environment ?version ?enabled ?pretty
    ?silent ?min_level ?drains ()

let diagnostic_count entries kind =
  List.fold_left
    (fun total (entry : Observe.Diagnostics.entry) ->
      if entry.kind = kind then total + entry.count else total)
    0 entries

let process_diagnostic_count kind =
  diagnostic_count (Observe.Diagnostics.snapshot ()) kind

let text_payload log =
  match Observe.Log.payload log with
  | Observe.Log.Text { tag; message } -> Some (tag, message)
  | Observe.Log.Free _ | Observe.Log.Structured _ -> None
