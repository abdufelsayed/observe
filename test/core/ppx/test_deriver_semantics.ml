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
type 'a box_list = Box_list of 'a list [@@deriving observe]
type 'a nested_box = Nested_box of 'a box [@@deriving observe]

type 'a parameter_tree =
  | Parameter_leaf of 'a
  | Parameter_branch of 'a parameter_tree list
[@@deriving observe]

type 'a inline_parameter =
  | Inline_parameter of { value : 'a; history : 'a list }
[@@deriving observe]

type mode = [ `Development | `Production of string ] [@@deriving observe]
type names = string list [@@deriving observe]
type maybe_name = string option [@@deriving observe]
type aliases = { names : names; maybe_name : maybe_name } [@@deriving observe]

let int_as_string =
  Observe.Type.map Observe.Type.string int_of_string string_of_int

type custom_repr = { custom_value : (int[@observe.repr int_as_string]) }
[@@deriving observe]

module Nobuiltin = struct
  let int_t = int_as_string

  type t = { value : (int[@observe.nobuiltin]) } [@@deriving observe]
end

module Wrapped = struct
  type 'a list = Wrapped of 'a [@@deriving observe]
end

type qualified_lookalike = { value : int Wrapped.list } [@@deriving observe]

type rich = {
  pair : int * string;
  optional : string option;
  items : string array;
  mode : mode;
  score : float;
  count : int64;
}
[@@deriving observe]

let round_trip description value =
  let encoded = Observe.Type.to_json_string description value in
  match Observe.Type.of_json_string description encoded with
  | Ok decoded -> (encoded, decoded)
  | Error (`Msg message) ->
      Alcotest.failf "derived JSON did not decode: %s" message

let check_matches_repr name description value =
  let specialized = Observe.Type.to_json_string description value in
  let generic =
    Repr.to_json_string ~minify:true (Observe.Type.repr description) value
  in
  Alcotest.(check string) name generic specialized

module Shadow = struct
  type nonrec shadowed = Wrapped of { previous : shadowed }
  [@@deriving observe]

  let round_trips () =
    let value = Wrapped { previous = 42 } in
    let encoded = Observe.Type.to_json_string shadowed_t value in
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
  Alcotest.(check bool) "parameter round-trip" true (decoded = value);
  let listed = Box_list [ "one"; "two" ] in
  let _, decoded = round_trip (box_list_t Observe.Type.string) listed in
  Alcotest.(check bool) "nested container parameter" true (decoded = listed);
  let nested = Nested_box (Box 42) in
  let _, decoded = round_trip (nested_box_t Observe.Type.int) nested in
  Alcotest.(check bool) "nested named parameter" true (decoded = nested);
  let recursive =
    Parameter_branch
      [ Parameter_leaf "one"; Parameter_branch [ Parameter_leaf "two" ] ]
  in
  let _, decoded =
    round_trip (parameter_tree_t Observe.Type.string) recursive
  in
  Alcotest.(check bool) "recursive parameter" true (decoded = recursive);
  let inline = Inline_parameter { value = 42; history = [ 1; 2 ] } in
  let _, decoded = round_trip (inline_parameter_t Observe.Type.int) inline in
  Alcotest.(check bool) "inline-record parameter" true (decoded = inline)

let test_custom_repr_attributes () =
  let custom = { custom_value = 42 } in
  check_matches_repr "observe.repr JSON parity" custom_repr_t custom;
  Alcotest.(check string)
    "observe.repr specialized projection" "{\"custom_value\":\"42\"}"
    (Observe.Type.to_json_string custom_repr_t custom);
  let nobuiltin = Nobuiltin.{ value = 42 } in
  check_matches_repr "observe.nobuiltin JSON parity" Nobuiltin.t nobuiltin;
  Alcotest.(check string)
    "observe.nobuiltin specialized projection" "{\"value\":\"42\"}"
    (Observe.Type.to_json_string Nobuiltin.t nobuiltin)

let test_qualified_builtin_lookalike () =
  let value = { value = Wrapped.Wrapped 42 } in
  check_matches_repr "qualified list is not Stdlib.list" qualified_lookalike_t
    value;
  let _, decoded = round_trip qualified_lookalike_t value in
  Alcotest.(check bool) "qualified lookalike round-trip" true (decoded = value)

let test_specialized_json_matches_repr () =
  let rich =
    {
      pair = (42, "quoted \"value\"\nnext");
      optional = Some "present";
      items = [| "one"; "two" |];
      mode = `Production "blue";
      score = 1.25;
      count = 9_007_199_254_740_993L;
    }
  in
  check_matches_repr "rich structures" rich_t rich;
  check_matches_repr "empty and omitted fields" rich_t
    { rich with optional = None; items = [||]; mode = `Development };
  check_matches_repr "recursive values" node_t
    (Branch [ Leaf "one"; Branch [ Leaf "two" ] ]);
  check_matches_repr "mutually recursive values" left_t
    (To_right (To_left Left_end));
  check_matches_repr "parameterized values"
    (box_t Observe.Type.string)
    (Box "value");
  check_matches_repr "invalid UTF-8 fallback" event_t (Cache_miss "\255")

let test_alias_field_omission () =
  check_matches_repr "empty alias fields are omitted" aliases_t
    { names = []; maybe_name = None };
  Alcotest.(check string)
    "empty aliases encode as an empty record" "{}"
    (Observe.Type.to_json_string aliases_t { names = []; maybe_name = None })

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
          Alcotest.test_case "custom Repr attributes" `Quick
            test_custom_repr_attributes;
          Alcotest.test_case "qualified builtin lookalike" `Quick
            test_qualified_builtin_lookalike;
          Alcotest.test_case "specialized JSON matches Repr" `Quick
            test_specialized_json_matches_repr;
          Alcotest.test_case "alias field omission" `Quick
            test_alias_field_omission;
        ] );
    ]
