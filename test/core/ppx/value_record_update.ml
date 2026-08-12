type t = { value : int }

let base = { value = 1 }
let _ = [%observe.value { base with value = 2 }]
