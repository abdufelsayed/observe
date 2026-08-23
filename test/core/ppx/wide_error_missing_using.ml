let log = Observe.Logs.create ~name:"event" ()
let _ = [%observe.set log error (Failure "missing interpreter")]
