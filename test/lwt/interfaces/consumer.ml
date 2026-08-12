module Observer = Observe.Make (Observe_lwt.IO)

let io =
  Observe_lwt.create
    ~clock:(fun () -> Ok (Observe.Timestamp.of_unix_ns 0L))
    ~console_style:(fun () -> Observe.Formatter.Plain)
    ~write_console:(fun _ -> Observe.IO.Accepted)
    ~can_lookup_context:(fun () -> true)
    ()

let observer = Observer.create io
let _ = observer
