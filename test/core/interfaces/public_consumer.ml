module IO = struct
  type +'a t = 'a
  type state = unit
  type 'a key = { mutable value : 'a option }

  let return value = value
  let bind value callback = callback value

  let observe callback =
    match callback () with
    | value -> Observe.IO.Returned value
    | exception raised ->
        Observe.IO.Raised (raised, Printexc.get_raw_backtrace ())

  let repropagate raised backtrace =
    Printexc.raise_with_backtrace raised backtrace

  let create_key () = { value = None }
  let get () key = key.value

  let with_binding () key value callback =
    let previous = key.value in
    key.value <- Some value;
    Fun.protect ~finally:(fun () -> key.value <- previous) callback

  let protect () ~finally callback = Fun.protect ~finally callback
  let is_control_exception () _ = false

  module Clock = struct
    let now () = Ok (Observe.Timestamp.of_unix_ns 0L)
    let monotonic_now () = Ok 0L
  end

  module Identity = struct
    let next () = Ok "consumer-operation"
  end

  module Console = struct
    let style () = Observe.Formatter.Plain
    let offer () _ = Observe.IO.Accepted
  end
end

module Observer = Observe.Make (IO)

type event = { value : int }

let event_t =
  let open Observe.Type in
  record "event" (fun value -> { value })
  |+ field "value" int (fun event -> event.value)
  |> sealr

type event_builder = {
  typed : event Observe.Schema.patch -> event Observe.Schema.patch;
}

let event_schema =
  Observe.Generated_runtime.record_schema event_t ~builder:(fun _ ->
      { typed = Fun.id })

let config = Observe.Config.create_exn ~service:"consumer" ()
let observer = Observer.create ()
let text = fun (m : Observe.Logs.builder) -> m.text ~tag:"consumer" "message"
let untyped = fun (m : Observe.Logs.builder) -> m.value (Observe.Value.int 1)

let typed =
 fun (m : Observe.Logs.builder) -> m.typed ~using:event_schema { value = 1 }

let pretty = Observe.Formatter.pretty Observe.Formatter.Plain
let wide = Observe.Logs.create_typed ~name:"consumer" ~using:event_schema ()
let correlated () = Observe.Logs.info ~operation:wide text

let operation callback =
  Observer.with_operation observer ~name:"consumer" ~using:event_schema callback

let typed_child callback =
  Observer.fork observer ~parent:wide ~name:"child" ~using:event_schema callback

let current () = Observe.Logs.current_typed ~using:event_schema

let inspect log =
  let body =
    match Observe.Log.event log with
    | Observe.Log.Text { tag; message } -> `Text (tag, message)
    | Observe.Log.Structured { origin; value } ->
        let origin =
          match origin with
          | Observe.Log.Open -> `Open
          | Observe.Log.Declared name -> `Declared name
        in
        `Structured (origin, Observe.Value.frozen_to_json_string value)
  in
  let operation =
    Option.map
      (fun operation ->
        ( Observe.Log.operation_name operation,
          Observe.Log.operation_id operation,
          Option.map Observe.Log.operation_reference_id
            (Observe.Log.operation_parent operation),
          Observe.Log.operation_duration_ns operation ))
      (Observe.Log.operation log)
  in
  ( Observe.Log.kind log,
    Option.map Observe.Log.operation_reference_id (Observe.Log.correlation log),
    operation,
    Observe.Log.timestamp log,
    Observe.Log.level log,
    body )

let extension_formatter =
  Observe.Formatter.create (fun log ->
      ignore (inspect log);
      Observe.Formatter.format Observe.Formatter.json log)

let extension_drain =
  Observe.Drain.create (fun log ->
      ignore (inspect log);
      Observe.Drain.Accepted)

let _ =
  ( config,
    observer,
    text,
    untyped,
    typed,
    pretty,
    correlated,
    operation,
    typed_child,
    current,
    extension_formatter,
    extension_drain )
