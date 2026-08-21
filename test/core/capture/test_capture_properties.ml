module Observer = Observe.Make (Test_io.IO)

let observer = Observer.create (Test_io.Host.create ())
let values = QCheck.Gen.list_small (QCheck.Gen.int_range (-100) 100)

let flat_case =
  let open QCheck.Gen in
  QCheck.make
    ~print:(fun (capacity, values) ->
      Format.asprintf "capacity=%d values=[%s]" capacity
        (String.concat ";" (List.map string_of_int values)))
    ~shrink:
      QCheck.Shrink.(
        pair (filter (fun value -> value > 0) int) (list ~shrink:int))
    (pair (int_range 1 16) values)

type nested_case = {
  outer_capacity : int;
  before : int list;
  inner_capacity : int;
  inner : int list;
  after : int list;
}

let print_values values =
  "[" ^ String.concat ";" (List.map string_of_int values) ^ "]"

let nested_case =
  let open QCheck.Gen in
  let generator =
    map
      (fun (outer_capacity, before, inner_capacity, inner, after) ->
        { outer_capacity; before; inner_capacity; inner; after })
      (tup5 (int_range 1 16) values (int_range 1 16) values values)
  in
  let positive_int =
    QCheck.Shrink.filter (fun value -> value > 0) QCheck.Shrink.int
  in
  let int_list = QCheck.Shrink.list ~shrink:QCheck.Shrink.int in
  let tuple_shrinker =
    QCheck.Shrink.tup5 positive_int int_list positive_int int_list int_list
  in
  let shrink case =
    QCheck.Iter.map
      (fun (outer_capacity, before, inner_capacity, inner, after) ->
        { outer_capacity; before; inner_capacity; inner; after })
      (tuple_shrinker
         ( case.outer_capacity,
           case.before,
           case.inner_capacity,
           case.inner,
           case.after ))
  in
  QCheck.make ~shrink
    ~print:(fun case ->
      Format.asprintf
        "outer_capacity=%d before=%s inner_capacity=%d inner=%s after=%s"
        case.outer_capacity (print_values case.before) case.inner_capacity
        (print_values case.inner) (print_values case.after))
    generator

let captured_text log =
  match Observe.Log.body log with
  | Observe.Log.Text { message; _ } -> int_of_string message
  | Observe.Log.Structured _ -> failwith "unexpected body"

let prop_capture_retains_prefix_and_conserves_offers =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:250)
    ~name:"capture retains the accepted prefix and diagnoses the remainder"
    flat_case (fun (capacity, values) ->
      let config = Test_io.config ~min_level:Observe.Level.Debug "property" in
      match
        Observer.with_capture observer config ~capacity (fun capture ->
            List.iter
              (fun value ->
                Observe.Logs.debug
                  (Test_io.text ~tag:"generated" (string_of_int value)))
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

let emit value =
  Observe.Logs.debug (Test_io.text ~tag:"generated" (string_of_int value))

let retained_values capture =
  List.map captured_text (Observe.Capture.logs capture)

let prefix capacity values =
  List.filteri (fun index _ -> index < min capacity (List.length values)) values

let overflow_count capture =
  Test_io.diagnostic_count
    (Observe.Capture.diagnostics capture)
    Observe.Diagnostics.Capture_overflow

let capture_matches ~capacity ~offered capture =
  retained_values capture = prefix capacity offered
  && List.length (Observe.Capture.logs capture) + overflow_count capture
     = List.length offered

let prop_nested_capture_routes_to_innermost_and_restores_outer =
  QCheck.Test.make ~count:(Test_profile.qcheck_count ~default:250)
    ~name:"nested capture owns inner offers and restores the outer route"
    nested_case (fun case ->
      let config = Test_io.config ~min_level:Observe.Level.Debug "property" in
      match
        Observer.with_capture observer config ~capacity:case.outer_capacity
          (fun outer_capture ->
            List.iter emit case.before;
            let inner_capture =
              match
                Observer.with_capture observer config
                  ~capacity:case.inner_capacity (fun inner_capture ->
                    List.iter emit case.inner;
                    inner_capture)
              with
              | Ok capture -> capture
              | Error _ -> failwith "nested capture was rejected"
            in
            List.iter emit case.after;
            (outer_capture, inner_capture))
      with
      | Error _ -> false
      | Ok (outer_capture, inner_capture) ->
          capture_matches ~capacity:case.outer_capacity
            ~offered:(case.before @ case.after) outer_capture
          && capture_matches ~capacity:case.inner_capacity ~offered:case.inner
               inner_capture)

let () =
  Alcotest.run "observe-capture-properties"
    [
      ( "pbt:observe:capture",
        [
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_capture_retains_prefix_and_conserves_offers;
          QCheck_alcotest.to_alcotest ~speed_level:`Quick
            prop_nested_capture_routes_to_innermost_and_restores_outer;
        ] );
    ]
