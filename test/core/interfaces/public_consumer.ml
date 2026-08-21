module IO = struct
  type +'a t = 'a
  type state = unit
  type 'a key = { mutable value : 'a option }

  let return value = value
  let bind value callback = callback value
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
let typed = fun (m : Observe.Logs.builder) -> m.typed event_schema { value = 1 }
let pretty = Observe.Formatter.pretty Observe.Formatter.Plain
let _ = (config, observer, text, untyped, typed, pretty)
