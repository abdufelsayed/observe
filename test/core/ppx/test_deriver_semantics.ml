type event =
  | User_login of { user_id : int; method_ : string }
  | Cache_miss of string
[@@deriving observe]

type envelope = { event : event; attempts : int list } [@@deriving observe]
type shadowed = int [@@deriving observe]
type node = Leaf of string | Branch of node list [@@deriving observe]

type left = Left_end | To_right of right
and right = Right_end | To_left of left [@@deriving observe]

type 'a box = Box of 'a [@@deriving observe]

let round_trip description value =
  let encoded = Observe.Type.to_json_string ~minify:true description value in
  match Observe.Type.of_json_string description encoded with
  | Ok decoded -> (encoded, decoded)
  | Error (`Msg message) ->
      Alcotest.failf "derived JSON did not decode: %s" message

module Shadow = struct
  type nonrec shadowed = Wrapped of { previous : shadowed }
  [@@deriving observe]

  let round_trips () =
    let value = Wrapped { previous = 42 } in
    let encoded = Observe.Type.to_json_string ~minify:true shadowed_t value in
    match Observe.Type.of_json_string shadowed_t encoded with
    | Ok decoded -> decoded = value
    | Error _ -> false
end

let test_inline_record_variant () =
  let value = User_login { user_id = 42; method_ = "oauth" } in
  let encoded, decoded = round_trip event_t value in
  Alcotest.(check bool) "variant round-trip" true (decoded = value);
  Alcotest.(check string)
    "semantic snapshot includes constructor"
    "{\"User_login\":{\"user_id\":42,\"method_\":\"oauth\"}}" encoded

let test_nested_named_descriptions () =
  let value = { event = Cache_miss "profile:42"; attempts = [ 1; 2; 3 ] } in
  let _, decoded = round_trip envelope_t value in
  Alcotest.(check bool) "nested derived round-trip" true (decoded = value)

let test_nonrecursive_shadowing () =
  Alcotest.(check bool)
    "nonrec field uses outer description" true (Shadow.round_trips ())

let test_recursive_descriptions () =
  let node = Branch [ Leaf "one"; Branch [ Leaf "two" ] ] in
  let _, decoded = round_trip node_t node in
  Alcotest.(check bool) "recursive round-trip" true (decoded = node);
  let linked = To_right (To_left Left_end) in
  let _, decoded = round_trip left_t linked in
  Alcotest.(check bool) "mutual round-trip" true (decoded = linked)

let test_parameterized_description () =
  let value = Box "value" in
  let _, decoded = round_trip (box_t Observe.Type.string) value in
  Alcotest.(check bool) "parameter round-trip" true (decoded = value)

let () =
  Alcotest.run "observe-ppx-deriver"
    [
      ( "behavior:observe:ppx-deriver",
        [
          Alcotest.test_case "inline-record variant" `Quick
            test_inline_record_variant;
          Alcotest.test_case "nested descriptions" `Quick
            test_nested_named_descriptions;
          Alcotest.test_case "nonrec shadowing" `Quick
            test_nonrecursive_shadowing;
          Alcotest.test_case "recursive descriptions" `Quick
            test_recursive_descriptions;
          Alcotest.test_case "parameterized description" `Quick
            test_parameterized_description;
        ] );
    ]
