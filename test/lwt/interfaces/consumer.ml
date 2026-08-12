module Observer = Observe.Make (Observe_lwt.IO)

let io =
  Observe_lwt.create
    ~clock:(fun () -> Ok (Observe.Instant.of_epoch_nanoseconds 0L))
    ~console_style:(fun () -> Observe.Formatter.Plain)
    ~write_console:(fun _ -> Observe.IO.Accepted)
    ()

let observer = Observer.create io
let _ = observer
