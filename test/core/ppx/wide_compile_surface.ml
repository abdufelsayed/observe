type phase = Started | Authorized of string [@@deriving observe]
type customer = { id : string; plan : string } [@@deriving observe]

type checkout_event = {
  cart_id : string;
  phase : phase;
  customer : customer;
  attempts : int;
}
[@@deriving observe]

let manual () =
  let checkout =
    Observe.Logs.create_typed ~name:"checkout" checkout_event_schema
  in
  Observe.Logs.set checkout (fun m ->
      m.typed (checkout_event_patch ~cart_id:"cart-1" ~phase:Started ()));
  Observe.Logs.set checkout (fun m ->
      m.typed
        (checkout_event_patch
           ~customer:(customer_patch ~id:"customer-1" ~plan:"pro" ())
           ~attempts:2 ()));
  [%observe.set checkout { phase = Authorized "authorization-1"; attempts = 3 }];
  [%observe.set checkout { customer = { id = "customer-2"; plan = "team" } }];
  [%observe.set checkout error Observe.Error.exn (Failure "declined")];
  Observe.Logs.emit checkout

let open_wide () =
  let log = Observe.Logs.create ~name:"open" () in
  [%observe.set
    log untyped
      { phase = "started"; customer = { id = "customer-1"; attempts = 2 } }];
  Observe.Logs.emit log

let anonymous_point () =
  [%observe.info
    untyped
      {
        phase = "started";
        customer = { id = "customer-1" };
        roles = [ "admin"; "billing" ];
        referral = None;
      }]

let () =
  ignore manual;
  ignore open_wide;
  ignore anonymous_point
