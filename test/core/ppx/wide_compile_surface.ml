type phase = Started | Authorized of string [@@deriving observe]
type customer = { id : string; plan : string } [@@deriving observe]

type checkout_event = {
  cart_id : string;
  phase : phase;
  customer : customer;
  attempts : int;
}
[@@deriving observe]

module Custom_description_name = struct
  type event = { value : int } [@@deriving observe { name = "custom" }]

  let description : event Observe.Type.t = custom
  let patch = event_patch ~value:1 ()
end

module External_description = struct
  module External = struct
    type t = int

    let t = Observe.Type.int
  end

  type event = { value : (External.t[@observe.repr External.t]) }
  [@@deriving observe]

  let patch = event_patch ~value:1 ()
end

module Hygienic_patch_binders = struct
  type event = { a : int; a_value : int } [@@deriving observe]

  let patch = event_patch ~a:1 ~a_value:2 ()
end

module Recursive_record = struct
  type node = { child : node } [@@deriving observe]

  let schema = node_schema
end

module Patch_type_name_collision = struct
  type patch = Keep
  type t = { value : int } [@@deriving observe]

  let authored = patch ~value:1 ()
  let domain = Keep
end

let manual () =
  let checkout =
    Observe.Logs.create_typed ~name:"checkout" ~using:checkout_event_schema ()
  in
  Observe.Logs.set checkout (fun m ->
      m.typed (checkout_event_patch ~cart_id:"cart-1" ~phase:Started ()));
  Observe.Logs.set checkout (fun m ->
      m.typed
        (checkout_event_patch
           ~customer:(customer_patch ~id:"customer-1" ~plan:"pro" ())
           ~attempts:2 ()));
  [%observe.set
    checkout typed { phase = Authorized "authorization-1"; attempts = 3 }];
  [%observe.set
    checkout typed { customer = { id = "customer-2"; plan = "team" } }];
  let backtrace = Printexc.get_callstack 8 in
  [%observe.set
    checkout error ~using:Observe.Error.exn ~backtrace (Failure "declined")];
  [%observe.set
    checkout error (Failure "declined again") ~backtrace
      ~using:Observe.Error.exn];
  [%observe.warn checkout "payment retry scheduled"];
  Observe.Logs.emit checkout

let open_wide () =
  let log = Observe.Logs.create ~name:"open" () in
  [%observe.set
    log { phase = "started"; customer = { id = "customer-1"; attempts = 2 } }];
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
  ignore Custom_description_name.description;
  ignore Custom_description_name.patch;
  ignore External_description.patch;
  ignore Hygienic_patch_binders.patch;
  ignore Recursive_record.schema;
  ignore Patch_type_name_collision.authored;
  ignore Patch_type_name_collision.domain;
  ignore manual;
  ignore open_wide;
  ignore anonymous_point
