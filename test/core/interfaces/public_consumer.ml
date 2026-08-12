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
    let now () = Ok (Observe.Instant.of_epoch_nanoseconds 0L)
  end

  module Console = struct
    let style () = Observe.Formatter.Plain
    let write () _ = Observe.IO.Accepted
  end
end

module Observer = Observe.Make (IO)

let config = Observe.Config.create_exn ~service:"consumer" ()
let observer = Observer.create ()
let text = Observe.Logs.text ~tag:"consumer" "message"
let free = Observe.Logs.free (fun () -> Observe.Value.int 1)
let structured = Observe.Logs.structured Observe.Type.int 1
let readable = Observe.Formatter.readable Observe.Formatter.Plain
let _ = (config, observer, text, free, structured, readable)
