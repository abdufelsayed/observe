let config = Observe.Config.create_exn ~service:"ready-consumer" ()
let initialize () = Observe_lwt_unix.init config
let flush () = Observe_lwt_unix.flush ()
let shutdown () = Observe_lwt_unix.shutdown ()

let advanced_flush () =
  let within = Observe_lwt_unix.Lifecycle.Duration.create_exn ~seconds:1. in
  Observe_lwt_unix.Lifecycle.flush ~within ()

let register_output () =
  Observe_lwt_unix.Lifecycle.Integration.register ~label:"custom-output"
    ~facts:(fun () -> Observe_lwt_unix.Lifecycle.Integration.Rejected_and_lost)
    ~flush:(fun () -> Lwt.return_unit)
    ~shutdown:(fun () -> Lwt.return_unit)

let inspect report =
  ( Observe_lwt_unix.Lifecycle.complete report,
    Observe_lwt_unix.Lifecycle.problems report )

let capture callback = Observe_lwt_unix.Test.with_capture_exn ~config callback

let operation callback =
  Observe_lwt_unix.with_operation ~name:"operation" callback

let child callback =
  Observe_lwt_unix.fork ~name:"child" ~error:Observe.Error.exn callback

let _ =
  ( initialize,
    flush,
    shutdown,
    advanced_flush,
    register_output,
    inspect,
    capture,
    operation,
    child )
