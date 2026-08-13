module Observer = Observe.Make (Test_io.IO)

type event = User_login of { user_id : int; method_ : string }
[@@deriving observe]

type access = Granted | Denied [@@deriving observe]
type deployment = [ `Development | `Staging | `Production ] [@@deriving observe]

type rich_event =
  | Session_started of {
      user_id : int;
      method_ : string;
      label : string;
      access : access;
      deployment : deployment;
      roles : string list;
      remembered : bool;
      provider : string option;
    }
[@@deriving observe]

type tree = Leaf of string | Branch of tree list [@@deriving observe]
type unsafe_variant = Unsafe

let timestamp = ref (Observe.Timestamp.of_unix_ns 37_425_612_000_000L)

let observer =
  let host = Test_io.Host.create ~now:(fun () -> Ok !timestamp) () in
  Observer.create host

let capture_one level message =
  let config = Test_io.config ~min_level:Observe.Level.Debug "example" in
  match
    Observer.with_capture observer config (fun capture ->
        Observe.Logs.emit ~level message;
        match Observe.Capture.logs capture with
        | [ log ] -> log
        | logs ->
            Alcotest.failf "expected one captured log, received %d"
              (List.length logs))
  with
  | Ok log -> log
  | Error Observe.IO_already_registered ->
      Alcotest.fail "I/O implementation unexpectedly conflicted"
  | Error (Observe.Invalid_capacity capacity) ->
      Alcotest.failf "unexpected invalid capacity: %d" capacity

let untyped make (builder : Observe.Logs.builder) = builder.untyped (make ())

let typed description value (builder : Observe.Logs.builder) =
  builder.typed description value

let format_pretty style log =
  match Observe.Formatter.format (Observe.Formatter.pretty style) log with
  | Ok output -> output
  | Error _ -> Alcotest.fail "pretty formatter rejected a valid log"

let pretty log = format_pretty Observe.Formatter.Plain log
let ansi_16 log = format_pretty Observe.Formatter.Ansi_16 log

let test_compact_information_text () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info (fun m ->
        m.text ~tag:"startup" "service %s (%d)" "ready" 2)
  in
  Alcotest.(check string)
    "compact information text" "10:23:45.612 INFO [startup] service ready (2)"
    (pretty log)

let check_text_level level expected =
  let log = capture_one level (Test_io.text ~tag:"tag" "message") in
  Alcotest.(check string) "level" expected (pretty log)

let test_visible_text_levels () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  check_text_level Observe.Level.Debug "10:23:45.612 DEBUG [tag] message";
  check_text_level Observe.Level.Info "10:23:45.612 INFO [tag] message";
  check_text_level Observe.Level.Warn "10:23:45.612 WARN [tag] message";
  check_text_level Observe.Level.Error "10:23:45.612 ERROR [tag] message"

let check_timestamp nanoseconds expected =
  timestamp := Observe.Timestamp.of_unix_ns nanoseconds;
  let log =
    capture_one Observe.Level.Info (Test_io.text ~tag:"time" "message")
  in
  Alcotest.(check string)
    "timestamp"
    (expected ^ " INFO [time] message")
    (pretty log)

let test_timestamp_boundaries () =
  check_timestamp 0L "00:00:00.000";
  check_timestamp 999_999L "00:00:00.000";
  check_timestamp 86_399_999_999_999L "23:59:59.999";
  check_timestamp (-1L) "23:59:59.999"

let test_text_control_escaping () =
  timestamp := Observe.Timestamp.of_unix_ns 0L;
  let log =
    capture_one Observe.Level.Warn
      (Test_io.text ~tag:"bad\ntag" "line one\r\nline two\027[31m")
  in
  Alcotest.(check string)
    "controls escaped"
    "00:00:00.000 WARN [bad\\ntag] line one\\r\\nline two\\u001b[31m"
    (pretty log)

let test_untyped_tree () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (untyped (fun () ->
           Observe.Value.object_
             [
               ("action", Observe.Value.string "user_login");
               ("user_id", Observe.Value.int 42);
             ]))
  in
  Alcotest.(check string)
    "untyped tree"
    "10:23:45.612 INFO [example]\n  ├─ action: \"user_login\"\n  └─ user_id: 42"
    (pretty log)

let test_typed_variant_tree () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (typed event_t (User_login { user_id = 42; method_ = "oauth" }))
  in
  Alcotest.(check string)
    "typed variant tree"
    "10:23:45.612 INFO [example]\n\
    \  └─ User_login\n\
    \     ├─ user_id: 42\n\
    \     └─ method_: \"oauth\""
    (pretty log)

let test_mixed_typed_structure () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (typed rich_event_t
         (Session_started
            {
              user_id = 42;
              method_ = "oauth";
              label = "Granted";
              access = Granted;
              deployment = `Development;
              roles = [ "admin"; "billing" ];
              remembered = true;
              provider = Some "github";
            }))
  in
  Alcotest.(check string)
    "mixed typed structure"
    "10:23:45.612 INFO [example]\n\
    \  └─ Session_started\n\
    \     ├─ user_id: 42\n\
    \     ├─ method_: \"oauth\"\n\
    \     ├─ label: \"Granted\"\n\
    \     ├─ access: Granted\n\
    \     ├─ deployment: `Development\n\
    \     ├─ roles: [\"admin\", \"billing\"]\n\
    \     ├─ remembered: true\n\
    \     └─ provider: \"github\""
    (pretty log)

let test_recursive_typed_structure () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (typed tree_t (Branch [ Leaf "one"; Branch [ Leaf "two" ] ]))
  in
  Alcotest.(check string)
    "recursive typed structure"
    "10:23:45.612 INFO [example]\n\
    \  └─ Branch\n\
    \     ├─ [0]\n\
    \     │  └─ Leaf: \"one\"\n\
    \     └─ [1]\n\
    \        └─ Branch\n\
    \           └─ [0]\n\
    \              └─ Leaf: \"two\""
    (pretty log)

let test_nested_values_and_strings () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let object_ fields = Observe.Value.object_ fields in
  let log =
    capture_one Observe.Level.Info
      (untyped (fun () ->
           object_
             [
               ("empty", Observe.Value.string "");
               ("space", Observe.Value.string " padded ");
               ("reserved", Observe.Value.string "true");
               ("multiline", Observe.Value.string "line one\nline two");
               ( "roles",
                 Observe.Value.list
                   [
                     Observe.Value.string "admin";
                     Observe.Value.string "billing";
                   ] );
               ( "metadata",
                 object_
                   [
                     ("provider", Observe.Value.string "github");
                     ("region", Observe.Value.string "eu-west-1");
                   ] );
               ( "items",
                 Observe.Value.list
                   [
                     object_ [ ("id", Observe.Value.int 1) ];
                     object_ [ ("id", Observe.Value.int 2) ];
                   ] );
             ]))
  in
  Alcotest.(check string)
    "nested values"
    "10:23:45.612 INFO [example]\n\
    \  ├─ empty: \"\"\n\
    \  ├─ space: \" padded \"\n\
    \  ├─ reserved: \"true\"\n\
    \  ├─ multiline: \"line one\\nline two\"\n\
    \  ├─ roles: [\"admin\", \"billing\"]\n\
    \  ├─ metadata\n\
    \  │  ├─ provider: \"github\"\n\
    \  │  └─ region: \"eu-west-1\"\n\
    \  └─ items\n\
    \     ├─ [0]\n\
    \     │  └─ id: 1\n\
    \     └─ [1]\n\
    \        └─ id: 2"
    (pretty log)

let test_unambiguous_literals () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (untyped (fun () ->
           Observe.Value.object_
             [
               ("ratio", Observe.Value.float 0.1);
               ("literal", Observe.Value.string "line\\nbreak");
               ("empty_object", Observe.Value.object_ []);
               ("empty_list", Observe.Value.list []);
             ]))
  in
  Alcotest.(check string)
    "literal rendering"
    "10:23:45.612 INFO [example]\n\
    \  ├─ ratio: 0.1\n\
    \  ├─ literal: \"line\\\\nbreak\"\n\
    \  ├─ empty_object: {}\n\
    \  └─ empty_list: []"
    (pretty log)

let test_manual_variant_control_escaping () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let description =
    Observe.Type.enum "unsafe_variant" [ ("Bad\027[31mName", Unsafe) ]
  in
  let log = capture_one Observe.Level.Info (typed description Unsafe) in
  Alcotest.(check string)
    "manual variant is console safe"
    "10:23:45.612 INFO [example]\n  └─ Bad\\u001b[31mName" (pretty log)

let test_empty_root_structure () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (untyped (fun () -> Observe.Value.object_ []))
  in
  Alcotest.(check string)
    "empty root object" "10:23:45.612 INFO [example]\n  └─ {}" (pretty log)

let test_direct_container_presentations () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let check name description value expected =
    let log = capture_one Observe.Level.Info (typed description value) in
    Alcotest.(check string) name expected (pretty log)
  in
  check "sequence preserves polymorphic variants"
    (Observe.Type.seq deployment_t)
    (List.to_seq [ `Development; `Staging ])
    "10:23:45.612 INFO [example]\n  ├─ [0]: `Development\n  └─ [1]: `Staging";
  let queue = Queue.create () in
  Queue.add Granted queue;
  Queue.add Denied queue;
  check "queue renders directly"
    (Observe.Type.queue access_t)
    queue "10:23:45.612 INFO [example]\n  └─ [Granted, Denied]";
  let stack = Stack.create () in
  Stack.push `Production stack;
  check "stack renders directly"
    (Observe.Type.stack deployment_t)
    stack "10:23:45.612 INFO [example]\n  └─ [`Production]";
  let table = Hashtbl.create 1 in
  Hashtbl.add table "permission" Granted;
  check "hash table renders directly"
    (Observe.Type.hashtbl Observe.Type.string access_t)
    table "10:23:45.612 INFO [example]\n  └─ [[\"permission\", Granted]]"

let test_explicit_repr_compatibility () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let opaque = Observe.Type.of_repr (Observe.Type.repr deployment_t) in
  let log = capture_one Observe.Level.Info (typed opaque `Development) in
  Alcotest.(check string)
    "opaque Repr projection exposes only its JSON meaning"
    "10:23:45.612 INFO [example]\n  └─ \"Development\"" (pretty log)

let test_ansi_16_text () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let check level tag message expected =
    let log = capture_one level (Test_io.text ~tag message) in
    Alcotest.(check string) "ANSI 16 text level" expected (ansi_16 log)
  in
  check Observe.Level.Debug "router" "matched route"
    "\027[90m10:23:45.612\027[0m \027[1;90mDEBUG\027[0m \
     \027[1;90m[router]\027[0m matched route";
  check Observe.Level.Info "auth" "user logged in"
    "\027[90m10:23:45.612\027[0m \027[1;96mINFO\027[0m \027[1;96m[auth]\027[0m \
     user logged in";
  check Observe.Level.Warn "cache" "cache miss"
    "\027[90m10:23:45.612\027[0m \027[1;93mWARN\027[0m \
     \027[1;93m[cache]\027[0m cache miss";
  check Observe.Level.Error "payment" "webhook failed"
    "\027[90m10:23:45.612\027[0m \027[1;91mERROR\027[0m \
     \027[1;91m[payment]\027[0m webhook failed"

let test_color_depths () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info (Test_io.text ~tag:"auth" "user logged in")
  in
  Alcotest.(check string)
    "ANSI 256"
    "\027[38;5;244m10:23:45.612\027[0m \027[1;38;5;39mINFO\027[0m \
     \027[1;38;5;39m[auth]\027[0m user logged in"
    (format_pretty Observe.Formatter.Ansi_256 log);
  Alcotest.(check string)
    "truecolor"
    "\027[38;2;111;119;130m10:23:45.612\027[0m \
     \027[1;38;2;14;165;233mINFO\027[0m \027[1;38;2;14;165;233m[auth]\027[0m \
     user logged in"
    (format_pretty Observe.Formatter.Truecolor log)

let test_ansi_16_structure () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (untyped (fun () ->
           Observe.Value.object_
             [
               ("action", Observe.Value.string "user_login");
               ("user_id", Observe.Value.int 42);
             ]))
  in
  Alcotest.(check string)
    "ANSI 16 structure"
    "\027[90m10:23:45.612\027[0m \027[1;96mINFO\027[0m \
     \027[1;96m[example]\027[0m\n\
    \  \027[90m├─\027[0m \027[95maction\027[0m\027[90m:\027[0m \
     \027[92m\"user_login\"\027[0m\n\
    \  \027[90m└─\027[0m \027[95muser_id\027[0m\027[90m:\027[0m \
     \027[93m42\027[0m"
    (ansi_16 log)

let test_ansi_16_typed_structure () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (typed event_t (User_login { user_id = 42; method_ = "oauth" }))
  in
  Alcotest.(check string)
    "constructor and fields"
    "\027[90m10:23:45.612\027[0m \027[1;96mINFO\027[0m \
     \027[1;96m[example]\027[0m\n\
    \  \027[90m└─\027[0m \027[1;95mUser_login\027[0m\n\
    \     \027[90m├─\027[0m \027[95muser_id\027[0m\027[90m:\027[0m \
     \027[93m42\027[0m\n\
    \     \027[90m└─\027[0m \027[95mmethod_\027[0m\027[90m:\027[0m \
     \027[92m\"oauth\"\027[0m"
    (ansi_16 log)

let check_formatter_error expected log =
  Alcotest.(check bool)
    "formatter error" true
    (Observe.Formatter.format
       (Observe.Formatter.pretty Observe.Formatter.Plain)
       log
    = Error expected)

let test_projection_failures () =
  timestamp := Observe.Timestamp.of_unix_ns 0L;
  let invalid_utf8 =
    capture_one Observe.Level.Info (Test_io.text ~tag:"invalid" "\255")
  in
  check_formatter_error Observe.Formatter.Invalid_utf8 invalid_utf8;
  let invalid_typed_utf8 =
    capture_one Observe.Level.Info (typed Observe.Type.string "\255")
  in
  check_formatter_error Observe.Formatter.Invalid_utf8 invalid_typed_utf8;
  Alcotest.(check bool)
    "Repr machine JSON remains available" true
    (match
       Observe.Formatter.format Observe.Formatter.json invalid_typed_utf8
     with
    | Ok encoded -> String.length encoded > 0
    | Error _ -> false);
  let non_finite =
    capture_one Observe.Level.Info (untyped (fun () -> Observe.Value.float nan))
  in
  check_formatter_error Observe.Formatter.Non_finite_float non_finite;
  let unsupported_repr =
    Repr.partially_abstract ~pp:Structural ~of_string:Structural ~json:Undefined
      ~bin:Structural ~unboxed_bin:Structural ~equal:Structural
      ~compare:Structural ~short_hash:Structural ~pre_hash:Structural Repr.int
  in
  let unsupported = Observe.Type.of_repr unsupported_repr in
  let unsupported = capture_one Observe.Level.Info (typed unsupported 1) in
  Alcotest.(check bool)
    "unsupported Repr JSON" true
    (Observe.Formatter.format Observe.Formatter.json unsupported
    = Error Observe.Formatter.Unsupported_value)

let () =
  Alcotest.run "observe-formatter"
    [
      ( "pretty text",
        [
          Alcotest.test_case "compact information text" `Quick
            test_compact_information_text;
          Alcotest.test_case "visible levels" `Quick test_visible_text_levels;
          Alcotest.test_case "timestamp boundaries" `Quick
            test_timestamp_boundaries;
          Alcotest.test_case "control escaping" `Quick
            test_text_control_escaping;
        ] );
      ( "pretty structure",
        [
          Alcotest.test_case "untyped tree" `Quick test_untyped_tree;
          Alcotest.test_case "typed variant tree" `Quick test_typed_variant_tree;
          Alcotest.test_case "mixed typed structure" `Quick
            test_mixed_typed_structure;
          Alcotest.test_case "recursive typed structure" `Quick
            test_recursive_typed_structure;
          Alcotest.test_case "nested values and strings" `Quick
            test_nested_values_and_strings;
          Alcotest.test_case "unambiguous literals" `Quick
            test_unambiguous_literals;
          Alcotest.test_case "manual variant control escaping" `Quick
            test_manual_variant_control_escaping;
          Alcotest.test_case "empty root structure" `Quick
            test_empty_root_structure;
          Alcotest.test_case "direct container presentations" `Quick
            test_direct_container_presentations;
          Alcotest.test_case "explicit Repr compatibility" `Quick
            test_explicit_repr_compatibility;
        ] );
      ( "ANSI styling",
        [
          Alcotest.test_case "tagged text" `Quick test_ansi_16_text;
          Alcotest.test_case "capability depths" `Quick test_color_depths;
          Alcotest.test_case "structured tree" `Quick test_ansi_16_structure;
          Alcotest.test_case "typed structured tree" `Quick
            test_ansi_16_typed_structure;
        ] );
      ( "failure containment",
        [
          Alcotest.test_case "projection failures" `Quick
            test_projection_failures;
        ] );
    ]
