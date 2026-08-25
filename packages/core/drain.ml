type acceptance = Accepted | Rejected
type t = { consume : Log.t -> acceptance; delivery_failed : bool Atomic.t }

let create consume = { consume; delivery_failed = Atomic.make false }
let offer t log = t.consume log

module Integration = struct
  let report_failure t =
    if Atomic.compare_and_set t.delivery_failed false true then
      Diagnostics.record Drain_delivery_failed
end
