let count_from_env ~default =
  match Sys.getenv_opt "OBSERVE_QCHECK_COUNT" with
  | None | Some "" -> default
  | Some value -> (
      match int_of_string_opt value with
      | Some count when count > 0 -> count
      | _ -> default)

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
  QCheck.Test.make ~count:(count_from_env ~default:200)
    ~name:"compare zero exactly when equal" level_pair (fun (left, right) ->
      Observe.Level.compare left right = 0 = Observe.Level.equal left right)

let prop_level_compare_is_antisymmetric =
  QCheck.Test.make ~count:(count_from_env ~default:200)
    ~name:"comparison signs are antisymmetric" level_pair (fun (left, right) ->
      Int.compare (Observe.Level.compare left right) 0
      = -Int.compare (Observe.Level.compare right left) 0)

let prop_instant_round_trips =
  QCheck.Test.make ~count:(count_from_env ~default:500)
    ~name:"epoch nanoseconds round-trip" QCheck.int64 (fun nanoseconds ->
      nanoseconds
      = Observe.Instant.(
          to_epoch_nanoseconds (of_epoch_nanoseconds nanoseconds)))

let prop_instant_compare_agrees_with_int64 =
  QCheck.Test.make
    ~count:(count_from_env ~default:300)
    ~name:"instant comparison agrees with epoch nanoseconds"
    QCheck.(pair int64 int64)
    (fun (left, right) ->
      Int.compare
        (Observe.Instant.compare
           (Observe.Instant.of_epoch_nanoseconds left)
           (Observe.Instant.of_epoch_nanoseconds right))
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
            prop_instant_round_trips;
            prop_instant_compare_agrees_with_int64;
          ] );
    ]
