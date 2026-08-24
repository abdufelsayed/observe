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

let point_logs () =
  [%observe.debug text ~tag:"checkout" "checkout route matched"];

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

  [%observe.info
    typed ~using:checkout_schema
      {
        cart_id = "cart-42";
        item_count = 2;
        phase = Completed;
        payment = Authorized { authorization_id = "auth-7" };
      }]

let manual_operations () =
  let untyped_log = Observe.Logs.create ~name:"open-checkout" () in
  [%observe.set
    untyped_log
      {
        cart_id = "cart-42";
        phase = "started";
        cart = { item_count = 2; currency = "USD" };
      }];
  [%observe.set
    untyped_log
      { phase = "authorized"; payment = { authorization_id = "auth-7" } }];
  Observe.Logs.emit untyped_log;

  (* Schema-locked wide log. Sparse patches remain ordinary typed OCaml. *)
  let typed_log =
    Observe.Logs.create_typed ~name:"typed-checkout" ~using:checkout_schema ()
  in
  [%observe.set
    typed_log typed { cart_id = "cart-42"; item_count = 2; phase = Started }];
  [%observe.set typed_log typed { phase = Inventory_reserved }];
  [%observe.set
    typed_log typed { payment = Authorized { authorization_id = "auth-7" } }];
  Observe.Logs.emit typed_log

let capture_payment () =
  let log = Observe.Logs.current () in
  [%observe.set
    log
      {
        provider = "example-pay";
        amount = 42.50;
        currency = "USD";
        status = "authorizing";
      }];
  [%observe.info log "payment attempt started"];
  [%observe.info text ~tag:"payment" "calling payment provider"];
  let open Lwt.Syntax in
  let* () = Lwt.pause () in
  raise
    (Payment_declined
       {
         code = "insufficient_funds";
         reason = "the payment provider declined the card";
       })

let checkout_operation () =
  let log = Observe.Logs.current () in

  [%observe.set
    log
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
        Observe_lwt_unix.fork ~parent:log ~name:"capture-payment"
          ~error:payment_error capture_payment)
      (function
        | Payment_declined { code; _ } ->
            [%observe.warn
              text ~tag:"checkout" "payment declined; checkout remains open"];
            [%observe.warn log "checkout retained after payment decline"];

            [%observe.set
              log
                {
                  phase = "payment_declined";
                  payment =
                    { status = "declined"; code = Observe.Type.string code };
                }];
            Observe.Logs.set_level log ~level:Observe.Level.Warn;
            Lwt.return_unit
        | error -> Lwt.fail error)
  in
  Lwt.return_unit

let main () =
  point_logs ();
  manual_operations ();
  Observe_lwt_unix.with_operation ~name:"checkout" ~error:payment_error
    checkout_operation

let () = Lwt_main.run (Lwt.finalize main Observe_lwt_unix.shutdown)
