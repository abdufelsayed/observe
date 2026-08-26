type acceptance = Accepted | Rejected

type t = {
  consume : Log.t -> acceptance;
  delivery_failed : bool Atomic.t;
  redaction : Log_redaction.t;
}

let create consume =
  {
    consume;
    delivery_failed = Atomic.make false;
    redaction = Log_redaction.none;
  }

let with_redaction ~redaction t =
  {
    t with
    redaction =
      Log_redaction.combine_exn ~policies:[ t.redaction; redaction ] ();
  }

let redaction t = t.redaction
let offer t log = t.consume log

module Integration = struct
  let report_failure t =
    if Atomic.compare_and_set t.delivery_failed false true then
      Diagnostics.record Drain_delivery_failed
end
