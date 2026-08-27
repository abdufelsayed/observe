type t = { keep : Log.t -> bool }

let create ~keep = { keep }
let keep t = t.keep
