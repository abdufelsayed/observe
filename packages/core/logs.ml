type message = Engine.message

let text = Engine.text
let text_lazy = Engine.text_lazy
let free = Engine.free
let structured = Engine.structured
let emit ~level message = Observer.emit ~level message
let debug message = emit ~level:Level.Debug message
let info message = emit ~level:Level.Info message
let warn message = emit ~level:Level.Warn message
let error message = emit ~level:Level.Error message
