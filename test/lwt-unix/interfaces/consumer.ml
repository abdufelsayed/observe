let config = Observe.Config.create_exn ~service:"ready-consumer" ()
let initialize () = Observe_lwt_unix.init config
let capture callback = Observe_lwt_unix.Test.with_capture config callback
let _ = (initialize, capture)
