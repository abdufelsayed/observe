type access = Granted | Denied of { reason : string; retryable : bool }
[@@deriving observe]

type environment = [ `Development | `Staging | `Production ]
[@@deriving observe]

type auth_method = Password | Passkey | Oauth of { provider : string }
[@@deriving observe]

type money = { currency : string; amount_minor : int } [@@deriving observe]

type line_item = { sku : string; quantity : int; unit_price : money }
[@@deriving observe]

type payment =
  | Authorized of { authorization_id : string }
  | Declined of { code : string; message : string }
[@@deriving observe]

type sync_source = [ `Postgres | `Mysql ] [@@deriving observe]
type sync_target = [ `S3 | `Filesystem ] [@@deriving observe]

type event =
  | User_login of {
      user_id : int;
      method_ : auth_method;
      label : string;
      access : access;
      environment : environment;
      roles : string list;
      remembered : bool;
      device_id : string option;
    }
  | Checkout_processed of {
      checkout_id : string;
      customer_id : string option;
      items : line_item list;
      total : money;
      payment : payment;
      latency_ms : float;
    }
  | Sync_failed of {
      source : sync_source;
      target : sync_target;
      attempts : int;
      error : string;
    }
[@@deriving observe]

let config =
  Observe.Config.create_exn ~service:"example" ~environment:"development"
    ~min_level:Observe.Level.Debug ()

let () = Observe_lwt_unix.init_exn config

let main () =
  Observe.Logs.debug
    (Observe.Logs.text_lazy ~tag:"router" (fun () ->
         "matched POST /api/checkout"));
  Observe.Logs.info (Observe.Logs.text ~tag:"auth" "user logged in");
  Observe.Logs.warn (Observe.Logs.text ~tag:"cache" "cache miss for user:42");
  Observe.Logs.error (Observe.Logs.text ~tag:"payment" "payment webhook failed");
  Observe.Logs.info
    (Observe.Logs.free
       [%observe.value
         {
           action = "request_finished";
           request_id = "req_01JQ8Y7A6M";
           route = "/api/checkout";
           status = 200;
           duration_ms = 12.8;
           cached = false;
         }]);
  Observe.Logs.info
    (Observe.Logs.structured event_t
       (User_login
          {
            user_id = 42;
            method_ = Oauth { provider = "github" };
            label = "Granted";
            access = Granted;
            environment = `Development;
            roles = [ "admin"; "billing" ];
            remembered = true;
            device_id = Some "device_7f3a";
          }));
  Observe.Logs.warn
    (Observe.Logs.structured event_t
       (Checkout_processed
          {
            checkout_id = "chk_01JQ8Z2C3A";
            customer_id = None;
            items =
              [
                {
                  sku = "ocaml-hoodie";
                  quantity = 1;
                  unit_price = { currency = "USD"; amount_minor = 8_900 };
                };
                {
                  sku = "camel-sticker";
                  quantity = 2;
                  unit_price = { currency = "USD"; amount_minor = 500 };
                };
              ];
            total = { currency = "USD"; amount_minor = 9_900 };
            payment =
              Declined
                { code = "insufficient_funds"; message = "card was declined" };
            latency_ms = 38.7;
          }));
  Observe.Logs.error
    (Observe.Logs.structured event_t
       (Sync_failed
          {
            source = `Postgres;
            target = `S3;
            attempts = 3;
            error = "connection timed out";
          }));
  Lwt.return_unit

let () = Lwt_main.run (main ())
