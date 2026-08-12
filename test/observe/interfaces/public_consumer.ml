module Runtime = struct
  type +'a t = 'a
  type context = unit
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
end

module Platform = struct
  type t = unit

  let console_style () = Observe.Formatter.Plain
  let now () = Ok (Observe.Instant.of_epoch_nanoseconds 0L)
  let write_console () _ = Observe.Platform.Accepted
end

module System = Observe.Runtime.Make (Runtime) (Platform)

let config = Observe.Config.create_exn ~service:"consumer" ()
let system = System.create ~runtime_context:() ~platform:()
let text = Observe.Logs.text ~tag:"consumer" "message"
let free = Observe.Logs.free (fun () -> Observe.Value.int 1)
let structured = Observe.Logs.structured Observe.Type.int 1
let readable = Observe.Formatter.readable Observe.Formatter.Plain
let _ = (config, system, text, free, structured, readable)
