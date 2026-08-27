let directory =
  match Array.to_list Sys.argv with
  | [ _; directory ] -> directory
  | _ -> "_build/observe-production-logs"

let deployment_context =
  Observe.Logs.Enricher.create_exn ~name:"deployment" (fun () ->
      Observe.Value.object_
        [
          ( "deployment",
            Observe.Value.object_
              [
                ("environment", Observe.Value.string "production");
                ("region", Observe.Value.string "eu-west-1");
              ] );
        ])

let limits =
  Observe.Logs.Limits.create_exn ~max_depth:16 ~max_object_fields:128
    ~max_collection_length:256 ~max_string_bytes:16_384 ~max_bytes_length:16_384
    ~max_nodes:4_096 ~max_total_bytes:262_144 ()

let global_redaction =
  let module R = Observe.Logs.Redaction in
  R.create_exn
    ~rules:
      [
        R.Rule.at
          (R.Path.fields [ "customer"; "email" ])
          (R.Action.mask
             (R.Mask.keep_suffix ~characters:3 ~hidden:(R.Mask.Collapse "***")
                ()));
        R.Rule.at (R.Path.fields [ "payment"; "access_token" ]) R.Action.remove;
      ]
    ()

let incident_redaction =
  let module R = Observe.Logs.Redaction in
  R.create_exn
    ~rules:[ R.Rule.at (R.Path.fields [ "customer"; "email" ]) R.Action.remove ]
    ()

let sampling =
  let module S = Observe.Logs.Sampling in
  S.create ~debug:(S.Rate.percent_exn 5.) ~info:(S.Rate.percent_exn 25.)
    ~stability:S.Correlation_stable ()

let retention =
  Observe.Logs.Retention.create ~keep:(fun log ->
      Observe.Level.equal (Observe.Log.level log) Observe.Level.Error
      ||
      match Observe.Log.kind log with
      | Observe.Log.Point _ -> false
      | Observe.Log.Wide { operation; _ } ->
          Int64.compare
            (Observe.Log.operation_duration_ns operation)
            250_000_000L
          >= 0)

let incident_route log =
  Observe.Level.equal (Observe.Log.level log) Observe.Level.Error

let print_problem = function
  | Observe_lwt_unix.Lifecycle.Rejected { output } ->
      Format.eprintf "output rejected work: %s@." output
  | Delivery_lost { output } ->
      Format.eprintf "output lost accepted work: %s@." output
  | Destination_failed { output } -> Format.eprintf "output failed: %s@." output
  | Timed_out { output } -> Format.eprintf "output timed out: %s@." output
  | Cancelled { output } -> Format.eprintf "output cancelled: %s@." output

let report boundary report =
  if Observe_lwt_unix.Lifecycle.complete report then
    Format.eprintf "%s completed@." boundary
  else List.iter print_problem (Observe_lwt_unix.Lifecycle.problems report)

let reserve_inventory () =
  let log = Observe.Logs.current () in
  [%observe.set
    log { inventory = { status = "reserved"; warehouse = "cai-1" } }];
  Lwt.return_unit

let checkout () =
  let log = Observe.Logs.current () in
  [%observe.set
    log
      {
        cart_id = "cart-42";
        customer = { email = "buyer@example.com"; plan = "team" };
        payment = { access_token = "secret-token"; provider = "example-pay" };
      }];
  [%observe.info log "checkout context assembled"];
  Observe_lwt_unix.fork ~name:"reserve-inventory" reserve_inventory

let emit_pressure () =
  for attempt = 1 to 32 do
    [%observe.error
      untyped
        {
          action = "payment_declined";
          attempt = Observe.Type.int attempt;
          customer = { email = "buyer@example.com" };
          payment = { access_token = "secret-token"; code = "declined" };
        }]
  done

let workload () =
  let open Lwt.Syntax in
  [%observe.info text ~tag:"startup" "production logging ready"];
  let* () = Observe_lwt_unix.with_operation ~name:"checkout" checkout in
  (* The incident output has capacity one. This synchronous burst deliberately
     demonstrates visible rejection without blocking the logging calls. *)
  emit_pressure ();
  let within = Observe_lwt_unix.Lifecycle.Duration.create_exn ~seconds:5. in
  let* flush_report = Observe_lwt_unix.Lifecycle.flush ~within () in
  report "flush" flush_report;
  [%observe.warn text ~tag:"lifecycle" "logging remains open after flush"];
  Lwt.return_unit

let main () =
  let open Lwt.Syntax in
  let* all_logs =
    Observe_fs_lwt_unix.create_exn
      ~dir:(Filename.concat directory "all")
      ~capacity:256 ()
  in
  let* incident_logs =
    Observe_fs_lwt_unix.create_exn
      ~dir:(Filename.concat directory "incidents")
      ~capacity:1 ()
  in
  let incident_logs =
    incident_logs
    |> Observe.Drain.with_route ~when_:incident_route
    |> Observe.Drain.with_redaction ~redaction:incident_redaction
  in
  let config =
    Observe.Config.create_exn ~service:"checkout" ~environment:"production"
      ~min_level:Observe.Level.Debug ~enrichers:[ deployment_context ] ~limits
      ~redaction:global_redaction ~sampling ~retention
      ~drains:[ all_logs; incident_logs ]
      ()
  in
  Observe_lwt_unix.init_exn config;
  Lwt.finalize workload (fun () ->
      let within = Observe_lwt_unix.Lifecycle.Duration.create_exn ~seconds:5. in
      let* shutdown_report = Observe_lwt_unix.Lifecycle.shutdown ~within () in
      report "shutdown" shutdown_report;
      Lwt.return_unit)

let () = Lwt_main.run (main ())
