module Observer = Observe.Make (Test_io.IO)

let observer = Observer.create (Test_io.Host.create ())

let count_from_env ~default =
  match Sys.getenv_opt "OBSERVE_QCHECK_COUNT" with
  | None | Some "" -> default
  | Some value -> (
      match int_of_string_opt value with
      | Some count when count > 0 -> count
      | _ -> default)

let case =
  let open QCheck.Gen in
  QCheck.make
    ~print:(fun (capacity, values) ->
      Printf.sprintf "capacity=%d values=[%s]" capacity
        (String.concat ";" (List.map string_of_int values)))
    (pair (int_range 1 16) (list_small (int_range (-100) 100)))

let captured_text log =
  match Observe.Log.payload log with
  | Observe.Log.Text { message; _ } -> int_of_string message
  | Observe.Log.Free _ | Observe.Log.Structured _ ->
      failwith "unexpected payload"

let prop_capture_retains_prefix_and_conserves_offers =
  QCheck.Test.make ~count:(count_from_env ~default:250)
    ~name:"capture retains the accepted prefix and diagnoses the remainder" case
    (fun (capacity, values) ->
      let config = Test_io.config ~min_level:Observe.Level.Debug "property" in
      match
        Observer.with_capture observer config ~capacity (fun capture ->
            List.iter
              (fun value ->
                Observe.Logs.debug
                  (Observe.Logs.text ~tag:"generated" (string_of_int value)))
              values;
            capture)
      with
      | Error _ -> false
      | Ok capture ->
          let retained = Observe.Capture.logs capture in
          let retained_values = List.map captured_text retained in
          let expected_count = min capacity (List.length values) in
          let expected_prefix =
            List.filteri (fun index _ -> index < expected_count) values
          in
          let overflow =
            Test_io.diagnostic_count
              (Observe.Capture.diagnostics capture)
              Observe.Diagnostics.Capture_overflow
          in
          retained_values = expected_prefix
          && List.length retained + overflow = List.length values)

let () =
  Alcotest.run "observe-capture-properties"
    [
      ( "pbt:observe:capture",
        [
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_capture_retains_prefix_and_conserves_offers;
        ] );
    ]
