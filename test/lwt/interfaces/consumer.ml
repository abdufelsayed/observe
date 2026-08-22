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

let request wide callback =
  Observer.manage observer wide ~error:Observe.Error.exn callback

let job wide callback = Observer.with_wide observer wide callback

let child parent callback =
  Observer.fork observer ~parent ~name:"child" ~error:Observe.Error.exn callback

let stream wide =
  let terminal = Observe.Logs.Terminal.create ~error:Observe.Error.exn wide in
  ( (fun () -> Observe.Logs.Terminal.complete terminal ()),
    fun raised backtrace ->
      Observe.Logs.Terminal.fail terminal ~backtrace raised )

let _ = (observer, request, job, child, stream)
