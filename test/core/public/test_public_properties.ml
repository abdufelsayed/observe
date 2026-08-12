let levels =
  [
    Observe.Level.Debug;
    Observe.Level.Info;
    Observe.Level.Warn;
    Observe.Level.Error;
  ]

let level =
  QCheck.make ~print:Observe.Level.to_string (QCheck.Gen.oneof_list levels)

let level_pair = QCheck.pair level level

let prop_level_compare_and_equal_agree =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:200)
    ~name:"compare zero exactly when equal" level_pair (fun (left, right) ->
      Observe.Level.compare left right = 0 = Observe.Level.equal left right)

let prop_level_compare_is_antisymmetric =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:200)
    ~name:"comparison signs are antisymmetric" level_pair (fun (left, right) ->
      Int.compare (Observe.Level.compare left right) 0
      = -Int.compare (Observe.Level.compare right left) 0)

let prop_timestamp_round_trips =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:500)
    ~name:"Unix nanoseconds round-trip" QCheck.int64 (fun nanoseconds ->
      nanoseconds = Observe.Timestamp.(to_unix_ns (of_unix_ns nanoseconds)))

let prop_timestamp_compare_agrees_with_int64 =
  QCheck.Test.make
    ~count:(Test_profile.qcheck_count ~default:300)
    ~name:"timestamp comparison agrees with Unix nanoseconds"
    QCheck.(pair int64 int64)
    (fun (left, right) ->
      Int.compare
        (Observe.Timestamp.compare
           (Observe.Timestamp.of_unix_ns left)
           (Observe.Timestamp.of_unix_ns right))
        0
      = Int.compare (Int64.compare left right) 0)

let () =
  Alcotest.run "observe-public-properties"
    [
      ( "pbt:observe:public-laws",
        List.map
          (QCheck_alcotest.to_alcotest ~speed_level:`Quick)
          [
            prop_level_compare_and_equal_agree;
            prop_level_compare_is_antisymmetric;
            prop_timestamp_round_trips;
            prop_timestamp_compare_agrees_with_int64;
          ] );
    ]
