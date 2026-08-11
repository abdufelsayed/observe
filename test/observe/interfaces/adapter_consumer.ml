module System =
  Observe.Runtime.Make (Observe_lwt.Runtime) (Observe_unix.Platform)

let system = System.create ~runtime_context:() ~platform:()
let config = Observe.Config.create_exn ~service:"adapter-consumer" ()
let initialize () = Observe_lwt_unix.init config
let capture callback = Observe_lwt_unix.Test.with_capture config callback
let _ = (system, initialize, capture)
