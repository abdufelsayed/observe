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

  (* Managed Lwt work uses the same wide-log lifecycle. Point logs inside the
     boundary correlate automatically, and an escaping exception would be
     contributed with its original backtrace before being re-propagated. *)
  let managed_checkout = Observe.Logs.create ~name:"managed-checkout" () in
  Observe_lwt_unix.manage managed_checkout ~error:Observe.Error.exn (fun () ->
      [%observe.set
        managed_checkout untyped { cart_id = "cart-42"; phase = "authorizing" }];
      [%observe.info text ~tag:"checkout" "starting payment child"];
      let open Lwt.Syntax in
      let* authorization_id =
        Observe_lwt_unix.fork ~parent:managed_checkout ~name:"capture-payment"
          ~error:Observe.Error.exn (fun payment ->
            [%observe.set
              payment untyped
                { payment = { status = "authorized"; id = "auth-8" } }];
            [%observe.info text ~tag:"payment" "payment captured"];
            Lwt.return "auth-8")
      in
      [%observe.set
        managed_checkout untyped
          { phase = "completed"; authorization_id = string authorization_id }];
      Lwt.return_unit)

let () = Lwt_main.run (Lwt.finalize main Observe_lwt_unix.shutdown)
