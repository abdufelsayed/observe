type message = Observe_engine.message

let text = Observe_engine.text
let text_lazy = Observe_engine.text_lazy
let free = Observe_engine.free
let structured = Observe_engine.structured
let emit ~level message = Observe_runtime.emit ~level message
let debug message = emit ~level:Observe_level.Debug message
let info message = emit ~level:Observe_level.Info message
let warn message = emit ~level:Observe_level.Warn message
let error message = emit ~level:Observe_level.Error message
