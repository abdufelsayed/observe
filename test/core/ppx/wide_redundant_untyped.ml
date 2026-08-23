let log = Observe.Logs.create ~name:"checkout" ()
let _ = [%observe.set log untyped { phase = "started" }]
