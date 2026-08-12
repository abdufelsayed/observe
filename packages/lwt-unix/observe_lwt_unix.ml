module System =
  Observe.Runtime.Make (Observe_lwt.Runtime) (Observe_unix.Platform)

let system = System.create ~runtime_context:() ~platform:()
let init config = System.init system config
let init_exn config = System.init_exn system config

module Test = struct
  exception Capture_error of Observe.Runtime.capture_error

  let with_capture config ?capacity callback =
    Lwt.bind (System.with_capture system config ?capacity callback) (function
      | Ok result -> Lwt.return result
      | Error error -> Lwt.fail (Capture_error error))
end
