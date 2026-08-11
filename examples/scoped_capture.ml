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

  let now () = Ok (Observe.Instant.of_epoch_nanoseconds 1L)
  let write_terminal () _ = Observe.Platform.Accepted
end

module System = Observe.Runtime.Make (Runtime) (Platform)

type event_data = { request_id : string; attempts : int } [@@deriving observe]
type event = Request of event_data [@@deriving observe]

let () =
  let system = System.create ~runtime_context:() ~platform:() in
  let config = Observe.Config.create_exn ~service:"example" () in
  let capture =
    match
      System.with_capture system config (fun capture ->
          Observe.Logs.info (Observe.Logs.text ~tag:"example" "text");
          Observe.Logs.info
            (Observe.Logs.free [%observe.value { kind = "free"; count = 2 }]);
          Observe.Logs.info
            (Observe.Logs.structured event_t
               (Request { request_id = "req-1"; attempts = 1 }));
          Runtime.return capture)
    with
    | Ok capture -> capture
    | Error Observe.Runtime.Runtime_already_registered ->
        failwith "another runtime already owns Observe"
    | Error (Observe.Runtime.Invalid_capacity capacity) ->
        invalid_arg (Printf.sprintf "invalid capture capacity: %d" capacity)
  in
  if List.length (Observe.Capture.logs capture) <> 3 then
    failwith "expected text, free-form, and structured logs"
