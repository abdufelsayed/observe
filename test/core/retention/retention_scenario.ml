module Observer = Observe.Make (Test_io.IO)
module Sampling = Observe.Logs.Sampling

let fail format = Format.kasprintf (fun message -> Alcotest.fail message) format
let rate percent = Sampling.Rate.percent_exn percent

let sampling ?debug ?info ?warn ?error ?stability () =
  Sampling.create ?debug ?info ?warn ?error ?stability ()

let text message (builder : Observe.Logs.builder) =
  builder.text ~tag:"retention" "%s" message

let structured ~secret ~plan (builder : Observe.Logs.builder) =
  let open Observe.Logs in
  builder.untyped
  |+ builder.field "secret" Observe.Type.string secret
  |+ builder.object_ "customer" (fun nested ->
      nested.untyped
      |+ nested.field "plan" Observe.Type.string plan
      |> nested.seal)
  |> builder.seal

let retained_drain retained =
  Observe.Drain.create (fun log ->
      retained := log :: !retained;
      Observe.Drain.Accepted)

let install ?(draw = fun () -> 0.) ?min_level ?sampling ?retention ?redaction
    drains =
  let host = Test_io.Host.create ~sampling_draw:draw () in
  let observer = Observer.create host in
  let config =
    Test_io.config ~console:Observe.Config.Silent ?min_level ~drains ?sampling
      ?retention ?redaction "retention"
  in
  Observer.init_exn observer config;
  observer

let diagnostic_count kind = Test_io.process_diagnostic_count kind

let test_rate_contract () =
  Alcotest.(check (float 0.))
    "never" 0.
    (Sampling.Rate.to_percent Sampling.Rate.never);
  Alcotest.(check (float 0.))
    "always" 100.
    (Sampling.Rate.to_percent Sampling.Rate.always);
  Alcotest.(check bool)
    "negative rejected" true
    (match Sampling.Rate.percent (-0.1) with
    | Error Out_of_range -> true
    | _ -> false);
  Alcotest.(check bool)
    "over 100 rejected" true
    (match Sampling.Rate.percent 100.1 with
    | Error Out_of_range -> true
    | _ -> false);
  Alcotest.(check bool)
    "NaN rejected" true
    (match Sampling.Rate.percent nan with
    | Error Not_finite -> true
    | _ -> false)

let test_inert () =
  let retained = ref [] in
  let draws = ref 0 in
  let draw () =
    incr draws;
    0.99
  in
  let sampling = sampling () in
  ignore (install ~draw ~sampling [ retained_drain retained ] : Observer.t);
  Observe.Logs.info (text "kept");
  Alcotest.(check int) "one retained" 1 (List.length !retained);
  Alcotest.(check int) "inert policy draws nothing" 0 !draws

let test_omitted_sampling () =
  let retained = ref [] in
  let draws = ref 0 in
  ignore
    (install
       ~draw:(fun () ->
         incr draws;
         0.99)
       [ retained_drain retained ]
      : Observer.t);
  Observe.Logs.info (text "kept-without-sampling");
  Alcotest.(check int) "omitted sampling retains" 1 (List.length !retained);
  Alcotest.(check int) "omitted sampling draws nothing" 0 !draws

let test_early_discard () =
  let retained = ref [] in
  let authored = ref false in
  let sampling = sampling ~info:Sampling.Rate.never () in
  ignore (install ~sampling [ retained_drain retained ] : Observer.t);
  Observe.Logs.info (fun builder ->
      authored := true;
      text "dropped" builder);
  Alcotest.(check bool) "author not evaluated" false !authored;
  Alcotest.(check int) "no retained logs" 0 (List.length !retained);
  Alcotest.(check int)
    "discard diagnosed" 1
    (diagnostic_count Observe.Diagnostics.Sampling_discarded)

let test_deterministic_draw () =
  let retained = ref [] in
  let draws = ref [ 0.09; 0.10; 0.99 ] in
  let draw () =
    match !draws with
    | value :: rest ->
        draws := rest;
        value
    | [] -> fail "draw sequence exhausted"
  in
  let sampling = sampling ~info:(rate 10.) () in
  ignore (install ~draw ~sampling [ retained_drain retained ] : Observer.t);
  Observe.Logs.info (text "kept");
  Observe.Logs.info (text "boundary-dropped");
  Observe.Logs.info (text "dropped");
  Alcotest.(check int) "strict probability boundary" 1 (List.length !retained)

let test_per_level () =
  let retained = ref [] in
  let sampling =
    sampling ~debug:Sampling.Rate.never ~info:Sampling.Rate.always
      ~warn:Sampling.Rate.never ~error:(rate 50.) ()
  in
  ignore
    (install ~min_level:Observe.Level.Debug
       ~draw:(fun () -> 0.75)
       ~sampling
       [ retained_drain retained ]
      : Observer.t);
  Observe.Logs.debug (text "debug");
  Observe.Logs.info (text "info");
  Observe.Logs.warn (text "warn");
  Observe.Logs.error (text "error");
  Alcotest.(check int)
    "only the configured level survives" 1 (List.length !retained);
  Alcotest.(check bool)
    "info survived" true
    (match Observe.Log.event (List.hd !retained) with
    | Observe.Log.Text { message = "info"; _ } -> true
    | Observe.Log.Text _ | Observe.Log.Structured _ -> false)

let test_error_default () =
  let retained = ref [] in
  let draws = ref 0 in
  let sampling = sampling ~info:Sampling.Rate.never () in
  ignore
    (install
       ~draw:(fun () ->
         incr draws;
         0.99)
       ~sampling
       [ retained_drain retained ]
      : Observer.t);
  Observe.Logs.error (text "kept-error");
  Alcotest.(check int) "error retained" 1 (List.length !retained);
  Alcotest.(check int) "error default is exact" 0 !draws

let test_error_override () =
  let retained = ref [] in
  let sampling = sampling ~error:Sampling.Rate.never () in
  ignore (install ~sampling [ retained_drain retained ] : Observer.t);
  Observe.Logs.error (text "dropped-error");
  Alcotest.(check int) "explicit error override" 0 (List.length !retained)

let test_wide_error_default () =
  let retained = ref [] in
  let draws = ref 0 in
  let sampling = sampling ~info:Sampling.Rate.never () in
  ignore
    (install
       ~draw:(fun () ->
         incr draws;
         0.99)
       ~sampling
       [ retained_drain retained ]
      : Observer.t);
  let wide = Observe.Logs.create ~name:"wide-error-default" () in
  Observe.Logs.set_level wide ~level:Observe.Level.Error;
  Observe.Logs.emit wide;
  Alcotest.(check int)
    "wide error retained by default" 1 (List.length !retained);
  Alcotest.(check int) "wide error default draws nothing" 0 !draws

let test_wide_error_override () =
  let retained = ref [] in
  let sampling = sampling ~error:Sampling.Rate.never () in
  ignore (install ~sampling [ retained_drain retained ] : Observer.t);
  let wide = Observe.Logs.create ~name:"wide-error-override" () in
  Observe.Logs.set_level wide ~level:Observe.Level.Error;
  Observe.Logs.emit wide;
  Alcotest.(check int)
    "explicit wide error override drops" 0 (List.length !retained);
  Alcotest.(check int)
    "wide error discard diagnosed" 1
    (diagnostic_count Observe.Diagnostics.Sampling_discarded)

let replacement_redaction () =
  let open Observe.Logs.Redaction in
  create_exn
    ~rules:
      [
        Rule.at (Path.fields [ "secret" ])
          (Action.replace (Observe.Value.string "safe"));
      ]
    ()

let matching_redaction () =
  let open Observe.Logs.Redaction in
  create_exn
    ~rules:
      [
        Rule.matching
          (Matcher.string_prefix "sk_live_")
          (Action.replace (Observe.Value.string "[secret]"));
      ]
    ()

let test_safe_retention () =
  let retained = ref [] in
  let inspected = ref false in
  let retention =
    Observe.Logs.Retention.create ~keep:(fun log ->
        let secret = Observe.Value.find [ "secret" ] (Observe.Log.fields log) in
        let plan =
          Observe.Value.find [ "customer"; "plan" ] (Observe.Log.fields log)
        in
        inspected := true;
        match
          ( Option.map Observe.Value.view secret,
            Option.map Observe.Value.view plan )
        with
        | Some (`String "safe"), Some (`String "enterprise") -> true
        | _ -> false)
  in
  let sampling = sampling ~info:Sampling.Rate.never () in
  ignore
    (install ~sampling ~retention ~redaction:(replacement_redaction ())
       [ retained_drain retained ]
      : Observer.t);
  Observe.Logs.info (structured ~secret:"unsafe" ~plan:"enterprise");
  Alcotest.(check bool) "retention inspected safe log" true !inspected;
  Alcotest.(check int) "safe rule rescued log" 1 (List.length !retained)

let test_failed_redaction_stays_safe () =
  let retained = ref [] in
  let retention_saw = ref None in
  let route_saw = ref None in
  let fallback = "[safe-fallback]" in
  let redaction =
    let open Observe.Logs.Redaction in
    create_exn
      ~rules:
        [
          Rule.at (Path.fields [ "secret" ])
            (Action.mask
               (Mask.custom ~fallback (fun _ -> failwith "mask failure")));
        ]
      ()
  in
  let read_secret log =
    Observe.Value.find [ "secret" ] (Observe.Log.fields log)
    |> Option.map Observe.Value.view
  in
  let retention =
    Observe.Logs.Retention.create ~keep:(fun log ->
        retention_saw := read_secret log;
        !retention_saw = Some (`String fallback))
  in
  let drain =
    retained_drain retained
    |> Observe.Drain.with_route ~when_:(fun log ->
        route_saw := read_secret log;
        !route_saw = Some (`String fallback))
  in
  ignore
    (install
       ~sampling:(sampling ~info:Sampling.Rate.never ())
       ~retention ~redaction [ drain ]
      : Observer.t);
  Observe.Logs.info (structured ~secret:"raw-secret" ~plan:"standard");
  Alcotest.(check bool)
    "retention sees only redaction fallback" true
    (!retention_saw = Some (`String fallback));
  Alcotest.(check bool)
    "route sees only redaction fallback" true
    (!route_saw = Some (`String fallback));
  Alcotest.(check int) "safe fallback is delivered" 1 (List.length !retained)

let test_safe_text_and_annotation_views () =
  let retained = ref [] in
  let routed_wide = ref [] in
  let text_seen = ref None in
  let annotation_seen = ref None in
  let retention =
    Observe.Logs.Retention.create ~keep:(fun log ->
        match Observe.Log.event log with
        | Observe.Log.Text { message; _ } ->
            text_seen := Some message;
            String.equal message "[secret]"
        | Observe.Log.Structured _ -> (
            match Observe.Log.kind log with
            | Observe.Log.Point _ -> false
            | Observe.Log.Wide { annotations; _ } -> (
                match annotations with
                | [ annotation ] ->
                    let message = Observe.Log.annotation_message annotation in
                    annotation_seen := Some message;
                    String.equal message "[secret]"
                | _ -> false)))
  in
  let wide_route log =
    match Observe.Log.kind log with
    | Observe.Log.Point _ -> false
    | Observe.Log.Wide { annotations; _ } -> (
        match annotations with
        | [ annotation ] ->
            String.equal (Observe.Log.annotation_message annotation) "[secret]"
        | _ -> false)
  in
  let routed_drain =
    retained_drain routed_wide |> Observe.Drain.with_route ~when_:wide_route
  in
  ignore
    (install
       ~sampling:(sampling ~info:Sampling.Rate.never ())
       ~retention ~redaction:(matching_redaction ())
       [ retained_drain retained; routed_drain ]
      : Observer.t);
  Observe.Logs.info (text "sk_live_point");
  let wide = Observe.Logs.create ~name:"safe-annotation" () in
  Observe.Logs.annotate wide ~level:Observe.Level.Info (fun () ->
      "sk_live_annotation");
  Observe.Logs.emit wide;
  Alcotest.(check (option string))
    "retention sees redacted text" (Some "[secret]") !text_seen;
  Alcotest.(check (option string))
    "retention sees redacted annotation" (Some "[secret]") !annotation_seen;
  Alcotest.(check int) "both safe logs rescued" 2 (List.length !retained);
  Alcotest.(check int) "safe annotation routed" 1 (List.length !routed_wide)

let test_retention_failure () =
  let retained = ref [] in
  let retention =
    Observe.Logs.Retention.create ~keep:(fun _ -> failwith "retention")
  in
  let sampling = sampling ~info:Sampling.Rate.never () in
  ignore (install ~sampling ~retention [ retained_drain retained ] : Observer.t);
  Observe.Logs.info (text "fail-open");
  Alcotest.(check int) "failure retains" 1 (List.length !retained);
  Alcotest.(check int)
    "failure diagnosed" 1
    (diagnostic_count Observe.Diagnostics.Retention_raised)

let test_source_failure () =
  let retained = ref [] in
  let sampling = sampling ~info:(rate 50.) () in
  ignore
    (install ~sampling ~draw:(fun () -> nan) [ retained_drain retained ]
      : Observer.t);
  Observe.Logs.info (text "invalid-kept");
  Alcotest.(check int) "invalid source fails open" 1 (List.length !retained);
  Alcotest.(check int)
    "invalid diagnosed" 1
    (diagnostic_count Observe.Diagnostics.Sampling_source_invalid)

let test_source_invalid_boundaries () =
  let retained = ref [] in
  let draws = ref [ -0.01; 1.; infinity ] in
  let draw () =
    match !draws with
    | value :: rest ->
        draws := rest;
        value
    | [] -> fail "draw sequence exhausted"
  in
  let sampling = sampling ~info:(rate 50.) () in
  ignore (install ~sampling ~draw [ retained_drain retained ] : Observer.t);
  Observe.Logs.info (text "negative");
  Observe.Logs.info (text "one");
  Observe.Logs.info (text "infinite");
  Alcotest.(check int) "all invalid draws fail open" 3 (List.length !retained);
  Alcotest.(check int)
    "each invalid draw is diagnosed" 3
    (diagnostic_count Observe.Diagnostics.Sampling_source_invalid)

let test_source_raise () =
  let retained = ref [] in
  let sampling = sampling ~info:(rate 50.) () in
  ignore
    (install ~sampling
       ~draw:(fun () -> failwith "draw")
       [ retained_drain retained ]
      : Observer.t);
  Observe.Logs.info (text "raised-kept");
  Alcotest.(check int) "raising source fails open" 1 (List.length !retained);
  Alcotest.(check int)
    "raising source diagnosed" 1
    (diagnostic_count Observe.Diagnostics.Sampling_source_raised)

let test_wide_source_failure ~stable ~draw ~diagnostic name =
  let retained = ref [] in
  let stable = if stable then Some Sampling.Correlation_stable else None in
  ignore
    (install
       ~sampling:(sampling ~info:(rate 50.) ?stability:stable ())
       ~draw
       [ retained_drain retained ]
      : Observer.t);
  let wide = Observe.Logs.create ~name () in
  Observe.Logs.set wide (fun builder ->
      let open Observe.Logs in
      builder.untyped
      |+ builder.field "phase" Observe.Type.string "finished"
      |> builder.seal);
  Observe.Logs.emit wide;
  Alcotest.(check int)
    "wide source failure fails open" 1 (List.length !retained);
  Alcotest.(check int)
    "wide source failure diagnosed once" 1
    (diagnostic_count diagnostic)

let test_source_control () =
  let sampling = sampling ~info:(rate 50.) () in
  ignore
    (install ~sampling
       ~draw:(fun () -> raise Test_io.Direct.Control)
       [ retained_drain (ref []) ]
      : Observer.t);
  Alcotest.check_raises "sampling preserves runtime control flow"
    Test_io.Direct.Control (fun () -> Observe.Logs.info (text "control"))

let test_retention_control () =
  let sampling = sampling ~info:Sampling.Rate.never () in
  let retention =
    Observe.Logs.Retention.create ~keep:(fun _ -> raise Test_io.Direct.Control)
  in
  ignore (install ~sampling ~retention [ retained_drain (ref []) ] : Observer.t);
  Alcotest.check_raises "retention preserves runtime control flow"
    Test_io.Direct.Control (fun () -> Observe.Logs.info (text "control"))

let test_capture_bypass () =
  let draws = ref 0 in
  let retention_calls = ref 0 in
  let route_calls = ref 0 in
  let host =
    Test_io.Host.create
      ~sampling_draw:(fun () ->
        incr draws;
        0.99)
      ()
  in
  let observer = Observer.create host in
  let retention =
    Observe.Logs.Retention.create ~keep:(fun _ ->
        incr retention_calls;
        false)
  in
  let drain =
    retained_drain (ref [])
    |> Observe.Drain.with_route ~when_:(fun _ ->
        incr route_calls;
        true)
  in
  let config =
    Test_io.config ~console:Observe.Config.Silent ~drains:[ drain ] ~retention
      ~sampling:(sampling ~info:Sampling.Rate.never ())
      "capture"
  in
  match
    Observer.with_capture observer ~config (fun capture ->
        Observe.Logs.info (text "captured");
        Observe.Capture.logs capture)
  with
  | Error _ -> fail "capture could not start"
  | Ok logs ->
      Alcotest.(check int) "capture bypasses sampling" 1 (List.length logs);
      Alcotest.(check int) "capture draws nothing" 0 !draws;
      Alcotest.(check int)
        "capture bypasses completion retention" 0 !retention_calls;
      Alcotest.(check int) "capture bypasses external routing" 0 !route_calls

let operation_named expected log =
  match Observe.Log.kind log with
  | Observe.Log.Wide { operation; _ } ->
      String.equal (Observe.Log.operation_name operation) expected
  | Observe.Log.Point _ -> false

let test_route_fanout () =
  let all = ref [] in
  let checkout = ref [] in
  let retention_calls = ref 0 in
  let retention =
    Observe.Logs.Retention.create ~keep:(fun _ ->
        incr retention_calls;
        false)
  in
  let all_drain =
    retained_drain all |> Observe.Drain.with_route ~when_:(fun _ -> true)
  in
  let checkout_drain =
    retained_drain checkout
    |> Observe.Drain.with_route ~when_:(operation_named "checkout")
  in
  let sampling = sampling ~info:(rate 50.) () in
  ignore
    (install ~sampling ~retention
       ~draw:(fun () -> 0.25)
       [ all_drain; checkout_drain ]
      : Observer.t);
  let wide = Observe.Logs.create ~name:"checkout" () in
  Observe.Logs.emit wide;
  Alcotest.(check int) "first route" 1 (List.length !all);
  Alcotest.(check int) "second route" 1 (List.length !checkout);
  Alcotest.(check int) "one final retention decision" 1 !retention_calls;
  Alcotest.(check bool)
    "same immutable log" true
    (List.hd !all == List.hd !checkout)

let test_point_retention_fanout () =
  let first = ref [] in
  let second = ref [] in
  let retention_calls = ref 0 in
  let retention =
    Observe.Logs.Retention.create ~keep:(fun _ ->
        incr retention_calls;
        false)
  in
  ignore
    (install
       ~sampling:(sampling ~info:(rate 50.) ())
       ~retention
       ~draw:(fun () -> 0.25)
       [ retained_drain first; retained_drain second ]
      : Observer.t);
  Observe.Logs.info (text "point-fanout");
  Alcotest.(check int) "point retention evaluated once" 1 !retention_calls;
  Alcotest.(check int) "first point drain receives" 1 (List.length !first);
  Alcotest.(check int) "second point drain receives" 1 (List.length !second);
  Alcotest.(check bool)
    "point fan-out shares immutable log" true
    (List.hd !first == List.hd !second)

let test_retention_precedes_base () =
  let retained = ref [] in
  let draws = ref 0 in
  let retention = Observe.Logs.Retention.create ~keep:(fun _ -> true) in
  let sampling = sampling ~info:(rate 50.) () in
  ignore
    (install ~sampling ~retention
       ~draw:(fun () ->
         incr draws;
         0.99)
       [ retained_drain retained ]
      : Observer.t);
  Observe.Logs.info (text "tail-kept");
  Alcotest.(check int)
    "completion policy retained the log" 1 (List.length !retained);
  Alcotest.(check int) "base draw was unnecessary" 0 !draws

let test_base_fallback () =
  let retained = ref [] in
  let calls = ref 0 in
  let retention =
    Observe.Logs.Retention.create ~keep:(fun _ ->
        incr calls;
        false)
  in
  let sampling = sampling ~info:(rate 50.) () in
  ignore
    (install ~sampling ~retention
       ~draw:(fun () -> 0.25)
       [ retained_drain retained ]
      : Observer.t);
  Observe.Logs.info (text "base-kept");
  Alcotest.(check int) "completion policy was consulted once" 1 !calls;
  Alcotest.(check int)
    "false deferred to the base keep" 1 (List.length !retained)

let test_wide_completion_retention () =
  let retained = ref [] in
  let retention =
    Observe.Logs.Retention.create ~keep:(fun log ->
        match Observe.Log.kind log with
        | Observe.Log.Wide { operation; _ } ->
            String.equal (Observe.Log.operation_name operation) "checkout"
            && Int64.compare (Observe.Log.operation_duration_ns operation) 0L
               > 0
            && Option.is_some
                 (Observe.Value.find [ "phase" ] (Observe.Log.fields log))
        | Observe.Log.Point _ -> false)
  in
  let sampling = sampling ~info:Sampling.Rate.never () in
  ignore (install ~sampling ~retention [ retained_drain retained ] : Observer.t);
  let wide = Observe.Logs.create ~name:"checkout" () in
  Observe.Logs.set wide (fun builder ->
      let open Observe.Logs in
      builder.untyped
      |+ builder.field "phase" Observe.Type.string "finished"
      |> builder.seal);
  Observe.Logs.emit wide;
  Alcotest.(check int)
    "final wide meaning rescued the observation" 1 (List.length !retained)

let test_wide_final_drop () =
  let retained = ref [] in
  let route_calls = ref 0 in
  let post_seal_authored = ref false in
  let drain =
    retained_drain retained
    |> Observe.Drain.with_route ~when_:(fun _ ->
        incr route_calls;
        true)
  in
  ignore
    (install ~sampling:(sampling ~info:Sampling.Rate.never ()) [ drain ]
      : Observer.t);
  let wide = Observe.Logs.create ~name:"wide-final-drop" () in
  Observe.Logs.emit wide;
  Observe.Logs.set wide (fun builder ->
      post_seal_authored := true;
      builder.untyped |> builder.seal);
  Alcotest.(check int) "wide final sampling drops" 0 (List.length !retained);
  Alcotest.(check int) "routing is skipped after final drop" 0 !route_calls;
  Alcotest.(check bool)
    "dropped wide remains sealed and lazy" false !post_seal_authored;
  Alcotest.(check int)
    "final wide discard diagnosed" 1
    (diagnostic_count Observe.Diagnostics.Sampling_discarded);
  Alcotest.(check int)
    "post-seal update remains diagnosed" 1
    (diagnostic_count Observe.Diagnostics.Post_seal_set)

let test_safe_route () =
  let retained = ref [] in
  let inspected = ref false in
  let route log =
    inspected := true;
    match
      Observe.Value.find [ "secret" ] (Observe.Log.fields log)
      |> Option.map Observe.Value.view
    with
    | Some (`String "safe") -> true
    | _ -> false
  in
  let drain =
    retained_drain retained |> Observe.Drain.with_route ~when_:route
  in
  ignore (install ~redaction:(replacement_redaction ()) [ drain ] : Observer.t);
  Observe.Logs.info (structured ~secret:"unsafe" ~plan:"standard");
  Alcotest.(check bool) "route inspected the safe floor" true !inspected;
  Alcotest.(check int) "safe route selected the drain" 1 (List.length !retained)

let test_nested_routes () =
  let retained = ref [] in
  let first_calls = ref 0 in
  let second_calls = ref 0 in
  let first _ =
    incr first_calls;
    false
  in
  let second _ =
    incr second_calls;
    true
  in
  let drain =
    retained_drain retained
    |> Observe.Drain.with_route ~when_:first
    |> Observe.Drain.with_route ~when_:second
  in
  ignore (install [ drain ] : Observer.t);
  Observe.Logs.info (text "not-routed");
  Alcotest.(check int) "first route evaluated once" 1 !first_calls;
  Alcotest.(check int) "conjunction short-circuited" 0 !second_calls;
  Alcotest.(check int) "drain not selected" 0 (List.length !retained)

let test_routed_redaction () =
  let floor = ref [] in
  let stricter = ref [] in
  let branch_redaction =
    let open Observe.Logs.Redaction in
    create_exn ~rules:[ Rule.at (Path.fields [ "secret" ]) Action.remove ] ()
  in
  let floor_drain = retained_drain floor in
  let stricter_drain =
    retained_drain stricter
    |> Observe.Drain.with_route ~when_:(fun _ -> true)
    |> Observe.Drain.with_redaction ~redaction:branch_redaction
  in
  ignore (install [ floor_drain; stricter_drain ] : Observer.t);
  Observe.Logs.info (structured ~secret:"safe-floor" ~plan:"standard");
  let floor_log = List.hd !floor in
  let stricter_log = List.hd !stricter in
  Alcotest.(check bool)
    "floor retains field" true
    (Option.is_some
       (Observe.Value.find [ "secret" ] (Observe.Log.fields floor_log)));
  Alcotest.(check bool)
    "branch removes field" true
    (Option.is_none
       (Observe.Value.find [ "secret" ] (Observe.Log.fields stricter_log)))

let test_routed_redaction_failure () =
  let open Observe.Logs.Redaction in
  let conflicting =
    create_exn
      ~rules:
        [
          Rule.matching
            (Matcher.string_prefix "secret-")
            (Action.replace (Observe.Value.string "[prefix]"));
          Rule.matching
            (Matcher.string_suffix "-value")
            (Action.replace (Observe.Value.string "[suffix]"));
        ]
      ()
  in
  let broken = ref [] in
  let healthy = ref [] in
  let broken_drain =
    retained_drain broken |> Observe.Drain.with_redaction ~redaction:conflicting
  in
  ignore (install [ broken_drain; retained_drain healthy ] : Observer.t);
  Observe.Logs.info (structured ~secret:"secret-source-value" ~plan:"standard");
  Alcotest.(check int) "failed branch is skipped" 0 (List.length !broken);
  Alcotest.(check int)
    "later drain receives safe floor" 1 (List.length !healthy);
  Alcotest.(check int)
    "branch projection failure diagnosed" 1
    (diagnostic_count Observe.Diagnostics.Drain_redaction_failed)

let test_route_failure () =
  let retained = ref [] in
  let broken =
    retained_drain (ref [])
    |> Observe.Drain.with_route ~when_:(fun _ -> failwith "route")
  in
  let healthy = retained_drain retained in
  ignore (install [ broken; healthy ] : Observer.t);
  Observe.Logs.info (text "healthy");
  Alcotest.(check int) "later drain still receives" 1 (List.length !retained);
  Alcotest.(check int)
    "route failure diagnosed" 1
    (diagnostic_count Observe.Diagnostics.Routing_raised)

let test_route_control () =
  let drain =
    retained_drain (ref [])
    |> Observe.Drain.with_route ~when_:(fun _ -> raise Test_io.Direct.Control)
  in
  ignore (install [ drain ] : Observer.t);
  Alcotest.check_raises "routing preserves runtime control flow"
    Test_io.Direct.Control (fun () -> Observe.Logs.info (text "control"))

let stable_fixture draw =
  let retained = ref [] in
  let draws = ref 0 in
  let sampling =
    sampling ~info:(rate 50.) ~stability:Sampling.Correlation_stable ()
  in
  let retention =
    Observe.Logs.Retention.create ~keep:(operation_named "child")
  in
  ignore
    (install ~sampling ~retention
       ~draw:(fun () ->
         incr draws;
         draw)
       [ retained_drain retained ]
      : Observer.t);
  let parent = Observe.Logs.create ~name:"parent" () in
  let child = Observe.Logs.create ~parent ~name:"child" () in
  let draws_before_decision = !draws in
  Observe.Logs.info ~operation:parent (text "point");
  Observe.Logs.emit child;
  Observe.Logs.emit parent;
  (!retained, draws_before_decision, !draws)

let test_stable_correlation () =
  let retained, draws_before_decision, draws = stable_fixture 0.75 in
  Alcotest.(check int) "wide creation does not draw" 0 draws_before_decision;
  Alcotest.(check int) "one root draw" 1 draws;
  Alcotest.(check int) "child rescued independently" 1 (List.length retained);
  Alcotest.(check bool)
    "retained child" true
    (operation_named "child" (List.hd retained))

let test_stable_exact_rates () =
  let retained = ref [] in
  let draws = ref 0 in
  let sampling =
    sampling ~info:Sampling.Rate.never ~error:Sampling.Rate.always
      ~stability:Sampling.Correlation_stable ()
  in
  ignore
    (install ~sampling
       ~draw:(fun () ->
         incr draws;
         0.5)
       [ retained_drain retained ]
      : Observer.t);
  let dropped = Observe.Logs.create ~name:"exact-zero" () in
  Observe.Logs.emit dropped;
  let kept = Observe.Logs.create ~name:"exact-one" () in
  Observe.Logs.set_level kept ~level:Observe.Level.Error;
  Observe.Logs.emit kept;
  Alcotest.(check int) "exact rates never draw" 0 !draws;
  Alcotest.(check int) "only exact one survives" 1 (List.length !retained)

let test_stable_concurrent_one_shot () =
  let thread_count = 16 in
  let started = Atomic.make 0 in
  let release = Atomic.make false in
  let draws = Atomic.make 0 in
  let delivered = Atomic.make 0 in
  let draw () =
    ignore (Atomic.fetch_and_add draws 1 : int);
    while not (Atomic.get release) do
      Thread.yield ()
    done;
    0.25
  in
  let drain =
    Observe.Drain.create (fun _ ->
        ignore (Atomic.fetch_and_add delivered 1 : int);
        Observe.Drain.Accepted)
  in
  let sampling =
    sampling ~info:(rate 50.) ~stability:Sampling.Correlation_stable ()
  in
  ignore (install ~sampling ~draw [ drain ] : Observer.t);
  let root = Observe.Logs.create ~name:"concurrent-root" () in
  let threads =
    Array.init thread_count (fun _ ->
        Thread.create
          (fun () ->
            ignore (Atomic.fetch_and_add started 1 : int);
            Observe.Logs.info ~operation:root (text "concurrent"))
          ())
  in
  while Atomic.get started < thread_count do
    Thread.yield ()
  done;
  Atomic.set release true;
  Array.iter Thread.join threads;
  Observe.Logs.emit root;
  Alcotest.(check int) "one source invocation" 1 (Atomic.get draws);
  Alcotest.(check int)
    "all related logs share the keep" (thread_count + 1) (Atomic.get delivered)

let test_independent_correlation () =
  let retained = ref [] in
  let draws = ref [ 0.25; 0.75; 0.25 ] in
  let calls = ref 0 in
  let draw () =
    incr calls;
    match !draws with
    | value :: rest ->
        draws := rest;
        value
    | [] -> fail "draw sequence exhausted"
  in
  let sampling = sampling ~info:(rate 50.) () in
  let retention = Observe.Logs.Retention.create ~keep:(fun _ -> false) in
  ignore
    (install ~sampling ~retention ~draw [ retained_drain retained ]
      : Observer.t);
  let parent = Observe.Logs.create ~name:"parent" () in
  let child = Observe.Logs.create ~parent ~name:"child" () in
  Observe.Logs.info ~operation:parent (text "point");
  Observe.Logs.emit child;
  Observe.Logs.emit parent;
  Alcotest.(check int) "independent draw per log" 3 !calls;
  Alcotest.(check int) "two independent keeps" 2 (List.length !retained);
  let point_parent =
    List.exists
      (fun log ->
        match Observe.Log.kind log with
        | Observe.Log.Point { correlation = Some operation } ->
            String.equal
              (Observe.Log.operation_reference_name operation)
              "parent"
        | Observe.Log.Point { correlation = None } | Observe.Log.Wide _ -> false)
      !retained
  in
  Alcotest.(check bool) "correlated point retained" true point_parent;
  Alcotest.(check bool)
    "parent wide retained" true
    (List.exists (operation_named "parent") !retained);
  Alcotest.(check bool)
    "child wide independently dropped" false
    (List.exists (operation_named "child") !retained)

let emit_same_instrumentation () =
  Observe.Logs.info (text "same-point");
  let wide = Observe.Logs.create ~name:"same-wide" () in
  Observe.Logs.emit wide

let test_composition_owned_policy selected =
  let logs = ref [] in
  if selected then
    let wide_only =
      retained_drain logs
      |> Observe.Drain.with_route ~when_:(operation_named "same-wide")
    in
    ignore
      (install ~sampling:(sampling ~info:Sampling.Rate.always ()) [ wide_only ]
        : Observer.t)
  else ignore (install [ retained_drain logs ] : Observer.t);
  emit_same_instrumentation ();
  if selected then (
    Alcotest.(check int) "composed policy selects one" 1 (List.length !logs);
    Alcotest.(check bool)
      "wide selected without authoring changes" true
      (operation_named "same-wide" (List.hd !logs)))
  else
    Alcotest.(check int) "default composition keeps both" 2 (List.length !logs)

let () =
  match Array.to_list Sys.argv with
  | [ _; "rate-contract" ] -> test_rate_contract ()
  | [ _; "inert" ] -> test_inert ()
  | [ _; "omitted-sampling" ] -> test_omitted_sampling ()
  | [ _; "early-discard" ] -> test_early_discard ()
  | [ _; "deterministic-draw" ] -> test_deterministic_draw ()
  | [ _; "per-level" ] -> test_per_level ()
  | [ _; "error-default" ] -> test_error_default ()
  | [ _; "error-override" ] -> test_error_override ()
  | [ _; "wide-error-default" ] -> test_wide_error_default ()
  | [ _; "wide-error-override" ] -> test_wide_error_override ()
  | [ _; "safe-retention" ] -> test_safe_retention ()
  | [ _; "failed-redaction-stays-safe" ] -> test_failed_redaction_stays_safe ()
  | [ _; "safe-text-and-annotation-views" ] ->
      test_safe_text_and_annotation_views ()
  | [ _; "retention-failure" ] -> test_retention_failure ()
  | [ _; "source-failure" ] -> test_source_failure ()
  | [ _; "source-invalid-boundaries" ] -> test_source_invalid_boundaries ()
  | [ _; "source-raise" ] -> test_source_raise ()
  | [ _; "wide-source-invalid" ] ->
      test_wide_source_failure ~stable:true
        ~draw:(fun () -> nan)
        ~diagnostic:Observe.Diagnostics.Sampling_source_invalid
        "stable-invalid-source"
  | [ _; "wide-source-raise" ] ->
      test_wide_source_failure ~stable:false
        ~draw:(fun () -> failwith "wide draw")
        ~diagnostic:Observe.Diagnostics.Sampling_source_raised
        "independent-raising-source"
  | [ _; "source-control" ] -> test_source_control ()
  | [ _; "retention-control" ] -> test_retention_control ()
  | [ _; "capture-bypass" ] -> test_capture_bypass ()
  | [ _; "route-fanout" ] -> test_route_fanout ()
  | [ _; "point-retention-fanout" ] -> test_point_retention_fanout ()
  | [ _; "retention-precedes-base" ] -> test_retention_precedes_base ()
  | [ _; "base-fallback" ] -> test_base_fallback ()
  | [ _; "wide-completion-retention" ] -> test_wide_completion_retention ()
  | [ _; "wide-final-drop" ] -> test_wide_final_drop ()
  | [ _; "safe-route" ] -> test_safe_route ()
  | [ _; "nested-routes" ] -> test_nested_routes ()
  | [ _; "routed-redaction" ] -> test_routed_redaction ()
  | [ _; "routed-redaction-failure" ] -> test_routed_redaction_failure ()
  | [ _; "route-failure" ] -> test_route_failure ()
  | [ _; "route-control" ] -> test_route_control ()
  | [ _; "stable-correlation" ] -> test_stable_correlation ()
  | [ _; "stable-exact-rates" ] -> test_stable_exact_rates ()
  | [ _; "stable-concurrent-one-shot" ] -> test_stable_concurrent_one_shot ()
  | [ _; "independent-correlation" ] -> test_independent_correlation ()
  | [ _; "composition-default" ] -> test_composition_owned_policy false
  | [ _; "composition-selected" ] -> test_composition_owned_policy true
  | _ -> fail "expected one retention scenario"
