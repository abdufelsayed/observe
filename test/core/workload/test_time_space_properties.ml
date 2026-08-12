module Observer = Observe.Make (Test_io.IO)

type clock_outcome = At of int64 | Unavailable | Raised
type event = { level : Observe.Level.t; clock : clock_outcome; value : int }

type temporal_case = {
  enabled : bool;
  min_level : Observe.Level.t;
  events : event list;
}

let current_clock = ref (At 0L)
let clock_calls = ref 0

let host =
  Test_io.Host.create
    ~now:(fun () ->
      incr clock_calls;
      match !current_clock with
      | At nanoseconds -> Ok (Observe.Instant.of_epoch_nanoseconds nanoseconds)
      | Unavailable -> Error Observe.IO.Unavailable
      | Raised -> failwith "generated clock failure")
    ()

let observer = Observer.create host

let levels =
  [
    Observe.Level.Debug;
    Observe.Level.Info;
    Observe.Level.Warn;
    Observe.Level.Error;
  ]

let level_gen = QCheck.Gen.oneof_list levels

let instant_gen =
  QCheck.Gen.oneof_weighted
    [
      (8, QCheck.Gen.int64);
      ( 2,
        QCheck.Gen.oneof_list
          [
            Int64.min_int;
            -86_400_000_000_001L;
            -1L;
            0L;
            999_999L;
            86_399_999_999_999L;
            86_400_000_000_000L;
            Int64.max_int;
          ] );
    ]

let clock_gen =
  QCheck.Gen.oneof_weighted
    [
      (8, QCheck.Gen.map (fun value -> At value) instant_gen);
      (1, QCheck.Gen.return Unavailable);
      (1, QCheck.Gen.return Raised);
    ]

let event_gen =
  QCheck.Gen.map3
    (fun level clock value -> { level; clock; value })
    level_gen clock_gen
    (QCheck.Gen.int_range (-10_000) 10_000)

let temporal_case =
  let generator =
    QCheck.Gen.map3
      (fun enabled min_level events -> { enabled; min_level; events })
      QCheck.Gen.bool level_gen
      (QCheck.Gen.list_size (QCheck.Gen.int_range 0 32) event_gen)
  in
  let shrink_clock = function
    | At 0L -> QCheck.Iter.empty
    | At _ | Unavailable | Raised -> QCheck.Iter.return (At 0L)
  in
  let shrink_level = function
    | Observe.Level.Debug -> QCheck.Iter.empty
    | Observe.Level.Info | Observe.Level.Warn | Observe.Level.Error ->
        QCheck.Iter.return Observe.Level.Debug
  in
  let shrink_event event =
    let open QCheck.Iter in
    append
      (map (fun level -> { event with level }) (shrink_level event.level))
      (append
         (map (fun clock -> { event with clock }) (shrink_clock event.clock))
         (map
            (fun value -> { event with value })
            (QCheck.Shrink.int event.value)))
  in
  let shrink case =
    let open QCheck.Iter in
    append
      (if case.enabled then return { case with enabled = false } else empty)
      (append
         (map
            (fun min_level -> { case with min_level })
            (shrink_level case.min_level))
         (map
            (fun events -> { case with events })
            (QCheck.Shrink.list ~shrink:shrink_event case.events)))
  in
  let print_clock = function
    | At value -> Int64.to_string value
    | Unavailable -> "unavailable"
    | Raised -> "raised"
  in
  let print_event event =
    Printf.sprintf "{%s;%s;%d}"
      (Observe.Level.to_string event.level)
      (print_clock event.clock) event.value
  in
  QCheck.make ~shrink
    ~print:(fun case ->
      Printf.sprintf "enabled=%b min=%s events=[%s]" case.enabled
        (Observe.Level.to_string case.min_level)
        (String.concat ";" (List.map print_event case.events)))
    generator

let admitted case event =
  case.enabled && Observe.Level.compare event.level case.min_level >= 0

let successful case event =
  admitted case event
  && match event.clock with At _ -> true | Unavailable | Raised -> false

let diagnostic_count capture kind =
  Test_io.diagnostic_count (Observe.Capture.diagnostics capture) kind

let prop_temporal_pipeline_obeys_stage_boundaries =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:400)
    ~name:"admission controls clocking, forcing, timestamps, and diagnostics"
    temporal_case (fun case ->
      clock_calls := 0;
      let forced = ref 0 in
      let config =
        Test_io.config ~enabled:case.enabled ~min_level:case.min_level
          "temporal"
      in
      match
        Observer.with_capture observer config
          ~capacity:(max 1 (List.length case.events))
          (fun capture ->
            List.iter
              (fun event ->
                current_clock := event.clock;
                Observe.Logs.emit ~level:event.level
                  (Observe.Logs.free (fun () ->
                       incr forced;
                       Observe.Value.int event.value)))
              case.events;
            capture)
      with
      | Error _ -> false
      | Ok capture ->
          let admitted_count =
            List.fold_left
              (fun total event ->
                if admitted case event then total + 1 else total)
              0 case.events
          in
          let successful_events = List.filter (successful case) case.events in
          let expected_instants =
            List.map
              (fun event ->
                match event.clock with
                | At value -> value
                | Unavailable | Raised -> assert false)
              successful_events
          in
          let retained_instants =
            List.map
              (fun log ->
                Observe.Instant.to_epoch_nanoseconds (Observe.Log.instant log))
              (Observe.Capture.logs capture)
          in
          let unavailable =
            List.fold_left
              (fun total event ->
                if admitted case event && event.clock = Unavailable then
                  total + 1
                else total)
              0 case.events
          in
          let raised =
            List.fold_left
              (fun total event ->
                if admitted case event && event.clock = Raised then total + 1
                else total)
              0 case.events
          in
          !clock_calls = admitted_count
          && !forced = List.length successful_events
          && retained_instants = expected_instants
          && diagnostic_count capture Observe.Diagnostics.Clock_unavailable
             = unavailable
          && diagnostic_count capture Observe.Diagnostics.Clock_raised = raised
          && diagnostic_count capture Observe.Diagnostics.Capture_overflow = 0)

let capacity_case =
  QCheck.make
    ~print:(fun (capacity, offered) ->
      Printf.sprintf "capacity=%d offered=%d" capacity offered)
    ~shrink:
      QCheck.Shrink.(
        pair
          (filter (fun value -> value > 0) int)
          (filter (fun value -> value >= 0) int))
    QCheck.Gen.(pair (int_range 1 64) (int_range 0 256))

let prop_capture_space_is_bounded_by_capacity =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:300)
    ~name:"capture state is bounded independently of offered volume"
    capacity_case (fun (capacity, offered) ->
      current_clock := At 0L;
      let config =
        Test_io.config ~min_level:Observe.Level.Debug "bounded-space"
      in
      match
        Observer.with_capture observer config ~capacity (fun capture ->
            for value = 1 to offered do
              Observe.Logs.debug
                (Observe.Logs.text ~tag:"space" (string_of_int value))
            done;
            capture)
      with
      | Error _ -> false
      | Ok capture ->
          let retained = List.length (Observe.Capture.logs capture) in
          let overflow =
            diagnostic_count capture Observe.Diagnostics.Capture_overflow
          in
          retained = min capacity offered
          && retained + overflow = offered
          && List.length (Observe.Capture.diagnostics capture)
             <= if overflow = 0 then 0 else 1)

let () =
  Alcotest.run "observe-time-space-properties"
    [
      ( "pbt:observe:time-space",
        [
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_temporal_pipeline_obeys_stage_boundaries;
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_capture_space_is_bounded_by_capacity;
        ] );
    ]
