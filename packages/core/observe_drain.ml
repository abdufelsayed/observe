type acceptance = Accepted | Rejected
type t = Drain of (Observe_log.t -> acceptance)

let create consume = Drain consume
let offer (Drain consume) log = consume log
