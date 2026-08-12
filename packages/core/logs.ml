type message = Message.t

let text = Message.text
let text_lazy = Message.text_lazy
let free = Message.free
let free_lazy = Message.free_lazy
let structured = Message.structured
let structured_lazy = Message.structured_lazy
let emit ~level message = Observer.emit ~level message
let debug message = emit ~level:Level.Debug message
let info message = emit ~level:Level.Info message
let warn message = emit ~level:Level.Warn message
let error message = emit ~level:Level.Error message
