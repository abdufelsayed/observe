type event =
  | User_login of { user_id : int; method_ : string }
  | Cache_miss of string
[@@deriving observe]

type envelope = { event : event; attempts : int list } [@@deriving observe]
type shadowed = int [@@deriving observe]

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
  let json = Yojson.Safe.from_string encoded in
  Alcotest.(check bool)
    "semantic snapshot includes constructor" true
    (match json with
    | `Assoc [ ("User_login", `Assoc fields) ] ->
        List.assoc_opt "user_id" fields = Some (`Int 42)
        && List.assoc_opt "method_" fields = Some (`String "oauth")
    | _ -> false)

let test_nested_named_descriptions () =
  let value = { event = Cache_miss "profile:42"; attempts = [ 1; 2; 3 ] } in
  let _, decoded = round_trip envelope_t value in
  Alcotest.(check bool) "nested derived round-trip" true (decoded = value)

let test_nonrecursive_shadowing () =
  Alcotest.(check bool)
    "nonrec field uses outer description" true (Shadow.round_trips ())

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
        ] );
    ]
