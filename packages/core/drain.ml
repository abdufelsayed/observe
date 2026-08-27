type acceptance = Accepted | Rejected

type t = {
  consume : Log.t -> acceptance;
  delivery_failed : bool Atomic.t;
  redaction : Log_redaction.t;
  route : (Log.t -> bool) option;
}

let create consume =
  {
    consume;
    delivery_failed = Atomic.make false;
    redaction = Log_redaction.none;
    route = None;
  }

let with_redaction ~redaction t =
  {
    t with
    redaction =
      Log_redaction.combine_exn ~policies:[ t.redaction; redaction ] ();
  }

let with_route ~when_ t =
  let route =
    match t.route with
    | None -> when_
    | Some previous -> fun log -> previous log && when_ log
  in
  { t with route = Some route }

let redaction t = t.redaction
let has_route t = Option.is_some t.route
let routed t log = match t.route with None -> true | Some route -> route log
let offer t log = t.consume log

module Integration = struct
  let report_failure t =
    if Atomic.compare_and_set t.delivery_failed false true then
      Diagnostics.record Drain_delivery_failed
end
