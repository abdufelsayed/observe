type acceptance = Accepted | Rejected
type t = Drain of (Log.t -> acceptance)

let create consume = Drain consume
let offer (Drain consume) log = consume log

module Integration = struct
  let report_failure () = Diagnostics.record Drain_delivery_failed
end
