let config = Observe.Config.create_exn ~service:"ready-consumer" ()
let initialize () = Observe_lwt_unix.init config
let flush () = Observe_lwt_unix.flush ()
let shutdown () = Observe_lwt_unix.shutdown ()
let capture callback = Observe_lwt_unix.Test.with_capture_exn config callback

let managed wide callback =
  Observe_lwt_unix.manage wide ~error:Observe.Error.exn callback

let scoped wide callback = Observe_lwt_unix.with_wide wide callback

let child parent callback =
  Observe_lwt_unix.fork ~parent ~name:"child" ~error:Observe.Error.exn callback

let _ = (initialize, flush, shutdown, capture, managed, scoped, child)
