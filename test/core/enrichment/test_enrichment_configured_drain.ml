module Observer = Observe.Make (Test_io.IO)

let field name log = Observe.Value.find [ name ] (Observe.Log.fields log)

let string_field name log =
  match Option.map Observe.Value.view (field name log) with
  | Some (`String value) -> value
  | _ -> Alcotest.failf "missing string field %s" name

let test_configured_drain_receives_enriched_log () =
  let received = ref [] in
  let drain =
    Observe.Drain.create (fun log ->
        received := log :: !received;
        Observe.Drain.Accepted)
  in
  let enricher =
    Observe.Logs.Enricher.create_exn ~name:"deployment" (fun () ->
        Observe.Value.object_
          [ ("region", Observe.Value.string "eu-central-1") ])
  in
  let config =
    Test_io.config ~console:Observe.Config.Silent ~drains:[ drain ]
      ~enrichers:[ enricher ] "configured-drain"
  in
  let observer = Observer.create (Test_io.Host.create ()) in
  Observer.init_exn observer config;
  Observe.Logs.info (fun m ->
      let open Observe.Logs in
      m.untyped |+ m.field "caller" Observe.Type.string "caller" |> m.seal);
  Observer.close observer;
  match !received with
  | [ delivered ] ->
      Alcotest.(check string)
        "caller field" "caller"
        (string_field "caller" delivered);
      Alcotest.(check string)
        "enriched field" "eu-central-1"
        (string_field "region" delivered)
  | delivered ->
      Alcotest.failf "expected one configured-drain delivery, received %d"
        (List.length delivered)

let () =
  Alcotest.run "observe-configured-drain-enrichment"
    [
      ( "integration",
        [
          Alcotest.test_case "configured drain receives enrichment" `Quick
            test_configured_drain_receives_enriched_log;
        ] );
    ]
