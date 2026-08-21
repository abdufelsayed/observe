type phase = Started | Inventory_reserved | Completed [@@deriving observe]

type payment =
  | Pending
  | Authorized of { authorization_id : string }
  | Declined of { code : string }
[@@deriving observe]

type checkout = {
  cart_id : string;
  item_count : int;
  phase : phase;
  payment : payment;
}
[@@deriving observe]

let config =
  Observe.Config.create_exn ~service:"checkout-example"
    ~environment:"development" ~min_level:Observe.Level.Debug ()

let () = Observe_lwt_unix.init_exn config

let main () =
  (* Text point log. *)
  [%observe.debug text ~tag:"checkout" "checkout route matched"];

  (* Anonymous point log. Self-describing values need no annotations. *)
  [%observe.info
    untyped
      {
        action = "cart_validated";
        cart_id = "cart-42";
        item_count = 2;
        customer = { plan = "team"; returning = true };
        applied_coupons = [ "WELCOME"; "TEAM" ];
        referral = None;
      }];

  (* Declared point log. The schema supplies every field description. *)
  [%observe.info
    typed checkout_schema
      {
        cart_id = "cart-42";
        item_count = 2;
        phase = Completed;
        payment = Authorized { authorization_id = "auth-7" };
      }];

  (* Open wide log. Each contribution uses the same anonymous value syntax. *)
  let open_checkout = Observe.Logs.create ~name:"open-checkout" () in
  [%observe.set
    open_checkout untyped
      {
        cart_id = "cart-42";
        phase = "started";
        cart = { item_count = 2; currency = "USD" };
      }];
  [%observe.set
    open_checkout untyped
      { phase = "authorized"; payment = { authorization_id = "auth-7" } }];
  Observe.Logs.emit open_checkout;

  (* Schema-locked wide log. Sparse patches remain ordinary typed OCaml. *)
  let typed_checkout =
    Observe.Logs.create_typed ~name:"typed-checkout" checkout_schema
  in
  [%observe.set
    typed_checkout { cart_id = "cart-42"; item_count = 2; phase = Started }];
  [%observe.set typed_checkout { phase = Inventory_reserved }];
  [%observe.set
    typed_checkout { payment = Authorized { authorization_id = "auth-7" } }];
  Observe.Logs.emit typed_checkout;
  Lwt.return_unit

let () = Lwt_main.run (Lwt.finalize main Observe_lwt_unix.shutdown)
