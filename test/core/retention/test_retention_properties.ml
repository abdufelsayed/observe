module Observer = Observe.Make (Test_io.IO)
module Sampling = Observe.Logs.Sampling

let draw_values = ref []
let retained = ref 0

let draw () =
  match !draw_values with
  | value :: rest ->
      draw_values := rest;
      float_of_int value /. 1_000.
  | [] -> failwith "sampling property exhausted its decisions"

let () =
  let drain =
    Observe.Drain.create (fun _ ->
        incr retained;
        Observe.Drain.Accepted)
  in
  let sampling = Sampling.create ~info:(Sampling.Rate.percent_exn 37.) () in
  let observer = Observer.create (Test_io.Host.create ~sampling_draw:draw ()) in
  Observer.init_exn observer
    (Test_io.config ~console:Observe.Config.Silent ~drains:[ drain ] ~sampling
       "retention-properties")

let decision_list =
  QCheck.make
    ~print:QCheck.Print.(list int)
    QCheck.Gen.(list_size (int_range 0 128) (int_range 0 999))

let prop_sampling_matches_strict_threshold =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:300)
    ~name:"base sampling retains exactly draws below its threshold"
    decision_list (fun decisions ->
      draw_values := decisions;
      retained := 0;
      List.iter
        (fun _ ->
          Observe.Logs.info (fun builder ->
              builder.text ~tag:"sampling-property" "value"))
        decisions;
      let expected =
        List.fold_left
          (fun count decision -> if decision < 370 then count + 1 else count)
          0 decisions
      in
      !draw_values = [] && !retained = expected)

let percentage =
  QCheck.make ~print:string_of_float
    QCheck.Gen.(
      map (fun value -> float_of_int value /. 1_000.) (int_range 0 100_000))

let prop_rate_round_trips =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:300)
    ~name:"every finite in-range percentage round-trips" percentage
    (fun percentage ->
      match Sampling.Rate.percent percentage with
      | Error _ -> false
      | Ok rate -> Sampling.Rate.to_percent rate = percentage)

let () =
  Alcotest.run "observe-retention-properties"
    [
      ( "pbt:observe:retention",
        [
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_sampling_matches_strict_threshold;
          QCheck_alcotest.to_alcotest ~speed_level:`Quick prop_rate_round_trips;
        ] );
    ]
