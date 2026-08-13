type message = Message.t

type builder = Message.builder = {
  text : 'a. tag:string -> ('a, Format.formatter, unit, message) format4 -> 'a;
  untyped : Value.t -> message;
  typed : 'a. 'a Type.t -> 'a -> message;
}

type author = builder -> message

let emit ~level author = Observer.emit ~level author
let debug author = emit ~level:Level.Debug author
let info author = emit ~level:Level.Info author
let warn author = emit ~level:Level.Warn author
let error author = emit ~level:Level.Error author
