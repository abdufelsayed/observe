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

exception Payment_declined of { code : string; reason : string }

let payment_error =
  Observe.Error.create (function
    | Payment_declined { code; reason } ->
        Observe.Error.roles ~kind:"payment_declined" ~code ~message:reason
          ~remediation:"ask the customer to use another payment method" ()
    | error ->
        Observe.Error.roles
          ~kind:(Printexc.exn_slot_name error)
          ~message:(Printexc.to_string error) ())

let config =
  Observe.Config.create_exn ~service:"checkout-example"
    ~environment:"development" ~min_level:Observe.Level.Debug ()

let () =
  Printexc.record_backtrace true;
  Observe_lwt_unix.init_exn config

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

  (* Managed Lwt work uses the same wide-log lifecycle. The payment child below
     fails after an asynchronous boundary. Observe records the escaping error
     on that child, emits it at [Error], restores the checkout scope, and
     re-propagates the same exception and backtrace. The checkout deliberately
     handles that domain failure and completes independently at [Warn]. *)
  let managed_checkout = Observe.Logs.create ~name:"managed-checkout" () in
  Observe_lwt_unix.manage managed_checkout ~error:payment_error (fun () ->
      [%observe.set
        managed_checkout untyped
          {
            cart_id = "cart-42";
            phase = "authorizing";
            customer = { plan = "team"; returning = true };
          }];
      [%observe.info text ~tag:"checkout" "starting payment attempt"];
      let open Lwt.Syntax in
      let* () =
        Lwt.catch
          (fun () ->
            Observe_lwt_unix.fork ~parent:managed_checkout
              ~name:"capture-payment" ~error:payment_error (fun payment ->
                [%observe.set
                  payment untyped
                    {
                      provider = "example-pay";
                      amount = 42.50;
                      currency = "USD";
                      status = "authorizing";
                    }];
                [%observe.info text ~tag:"payment" "calling payment provider"];
                let* () = Lwt.pause () in
                raise
                  (Payment_declined
                     {
                       code = "insufficient_funds";
                       reason = "the payment provider declined the card";
                     })))
          (function
            | Payment_declined { code; _ } ->
                (* [fork] has restored the parent scope before this handler.
                   This point log therefore correlates with [managed_checkout],
                   not with the completed payment child. *)
                [%observe.warn
                  text ~tag:"checkout"
                    "payment declined; keeping the checkout open"];
                [%observe.set
                  managed_checkout untyped
                    {
                      phase = "payment_declined";
                      payment = { status = "declined"; code = string code };
                    }];
                Observe.Logs.set_level managed_checkout Observe.Level.Warn;
                Lwt.return_unit
            | error -> Lwt.fail error)
      in
      Lwt.return_unit)

let () = Lwt_main.run (Lwt.finalize main Observe_lwt_unix.shutdown)
