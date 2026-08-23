module Observer = Observe.Make (Observe_lwt.IO)

let io =
  Observe_lwt.create
    ~clock:(fun () -> Ok (Observe.Timestamp.of_unix_ns 0L))
    ~monotonic_now:(fun () -> Ok 0L)
    ~next_id:(fun () -> Ok "consumer-operation")
    ~console_style:(fun () -> Observe.Formatter.Plain)
    ~offer_console:(fun _ -> Observe.IO.Accepted)
    ~can_lookup_context:(fun () -> true)
    ()

let observer = Observer.create io
let request callback = Observer.with_operation observer ~name:"request" callback

let child parent callback =
  Observer.fork observer ~parent ~name:"child" callback

let _ = (observer, request, child)
