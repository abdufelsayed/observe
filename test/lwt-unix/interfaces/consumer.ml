let config = Observe.Config.create_exn ~service:"ready-consumer" ()
let initialize () = Observe_lwt_unix.init config
let flush () = Observe_lwt_unix.flush ()
let shutdown () = Observe_lwt_unix.shutdown ()
let capture callback = Observe_lwt_unix.Test.with_capture_exn config callback
let _ = (initialize, flush, shutdown, capture)
