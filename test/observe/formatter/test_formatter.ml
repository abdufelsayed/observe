module System =
  Observe.Runtime.Make (Test_runtime.Runtime) (Test_runtime.Platform)

type event = User_login of { user_id : int; method_ : string }
[@@deriving observe]

let instant = ref (Observe.Instant.of_epoch_nanoseconds 37_425_612_000_000L)

let system =
  let platform = Test_runtime.Platform.create ~now:(fun () -> Ok !instant) () in
  System.create ~runtime_context:() ~platform

let capture_one level message =
  let config = Test_runtime.config ~min_level:Observe.Level.Debug "example" in
  match
    System.with_capture system config (fun capture ->
        Observe.Logs.emit ~level message;
        match Observe.Capture.logs capture with
        | [ log ] -> log
        | logs ->
            Alcotest.failf "expected one captured log, received %d"
              (List.length logs))
  with
  | Ok log -> log
  | Error Observe.Runtime.Runtime_already_registered ->
      Alcotest.fail "runtime unexpectedly conflicted"
  | Error (Observe.Runtime.Invalid_capacity capacity) ->
      Alcotest.failf "unexpected invalid capacity: %d" capacity

let format_readable style log =
  match Observe.Formatter.format (Observe.Formatter.readable style) log with
  | Ok output -> output
  | Error _ -> Alcotest.fail "readable formatter rejected a valid log"

let readable log = format_readable Observe.Formatter.Plain log
let ansi_16 log = format_readable Observe.Formatter.Ansi_16 log

let test_compact_information_text () =
  instant := Observe.Instant.of_epoch_nanoseconds 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (Observe.Logs.text ~tag:"startup" "service ready")
  in
  Alcotest.(check string)
    "compact information text" "10:23:45.612 INFO [startup] service ready"
    (readable log)

let check_text_level level expected =
  let log = capture_one level (Observe.Logs.text ~tag:"tag" "message") in
  Alcotest.(check string) "level" expected (readable log)

let test_visible_text_levels () =
  instant := Observe.Instant.of_epoch_nanoseconds 37_425_612_000_000L;
  check_text_level Observe.Level.Debug "10:23:45.612 DEBUG [tag] message";
  check_text_level Observe.Level.Info "10:23:45.612 INFO [tag] message";
  check_text_level Observe.Level.Warn "10:23:45.612 WARN [tag] message";
  check_text_level Observe.Level.Error "10:23:45.612 ERROR [tag] message"

let check_timestamp nanoseconds expected =
  instant := Observe.Instant.of_epoch_nanoseconds nanoseconds;
  let log =
    capture_one Observe.Level.Info (Observe.Logs.text ~tag:"time" "message")
  in
  Alcotest.(check string)
    "timestamp"
    (expected ^ " INFO [time] message")
    (readable log)

let test_timestamp_boundaries () =
  check_timestamp 0L "00:00:00.000";
  check_timestamp 999_999L "00:00:00.000";
  check_timestamp 86_399_999_999_999L "23:59:59.999";
  check_timestamp (-1L) "23:59:59.999"

let test_text_control_escaping () =
  instant := Observe.Instant.of_epoch_nanoseconds 0L;
  let log =
    capture_one Observe.Level.Warn
      (Observe.Logs.text ~tag:"bad\ntag" "line one\r\nline two\027[31m")
  in
  Alcotest.(check string)
    "controls escaped"
    "00:00:00.000 WARN [bad\\ntag] line one\\r\\nline two\\u001b[31m"
    (readable log)

let test_free_form_tree () =
  instant := Observe.Instant.of_epoch_nanoseconds 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (Observe.Logs.free (fun () ->
           Observe.Value.object_
             [
               ("action", Observe.Value.string "user_login");
               ("user_id", Observe.Value.int 42);
             ]))
  in
  Alcotest.(check string)
    "free-form tree"
    "10:23:45.612 INFO [example]\n  ├─ action: user_login\n  └─ user_id: 42"
    (readable log)

let test_typed_variant_tree () =
  instant := Observe.Instant.of_epoch_nanoseconds 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (Observe.Logs.structured event_t
         (User_login { user_id = 42; method_ = "oauth" }))
  in
  Alcotest.(check string)
    "typed variant tree"
    "10:23:45.612 INFO [example]\n\
    \  └─ User_login\n\
    \     ├─ user_id: 42\n\
    \     └─ method_: oauth"
    (readable log)

let test_nested_values_and_strings () =
  instant := Observe.Instant.of_epoch_nanoseconds 37_425_612_000_000L;
  let object_ fields = Observe.Value.object_ fields in
  let log =
    capture_one Observe.Level.Info
      (Observe.Logs.free (fun () ->
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
    \  ├─ roles: [admin, billing]\n\
    \  ├─ metadata\n\
    \  │  ├─ provider: github\n\
    \  │  └─ region: eu-west-1\n\
    \  └─ items\n\
    \     ├─ [0]\n\
    \     │  └─ id: 1\n\
    \     └─ [1]\n\
    \        └─ id: 2"
    (readable log)

let test_unambiguous_literals () =
  instant := Observe.Instant.of_epoch_nanoseconds 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (Observe.Logs.free (fun () ->
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
    (readable log)

let test_empty_root_structure () =
  instant := Observe.Instant.of_epoch_nanoseconds 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (Observe.Logs.free (fun () -> Observe.Value.object_ []))
  in
  Alcotest.(check string)
    "empty root object" "10:23:45.612 INFO [example]\n  └─ {}" (readable log)

let test_ansi_16_text () =
  instant := Observe.Instant.of_epoch_nanoseconds 37_425_612_000_000L;
  let check level tag message expected =
    let log = capture_one level (Observe.Logs.text ~tag message) in
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
  instant := Observe.Instant.of_epoch_nanoseconds 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (Observe.Logs.text ~tag:"auth" "user logged in")
  in
  Alcotest.(check string)
    "ANSI 256"
    "\027[38;5;244m10:23:45.612\027[0m \027[1;38;5;39mINFO\027[0m \
     \027[1;38;5;39m[auth]\027[0m user logged in"
    (format_readable Observe.Formatter.Ansi_256 log);
  Alcotest.(check string)
    "truecolor"
    "\027[38;2;111;119;130m10:23:45.612\027[0m \
     \027[1;38;2;14;165;233mINFO\027[0m \027[1;38;2;14;165;233m[auth]\027[0m \
     user logged in"
    (format_readable Observe.Formatter.Truecolor log)

let test_ansi_16_structure () =
  instant := Observe.Instant.of_epoch_nanoseconds 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (Observe.Logs.free (fun () ->
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
     \027[92muser_login\027[0m\n\
    \  \027[90m└─\027[0m \027[95muser_id\027[0m\027[90m:\027[0m \
     \027[93m42\027[0m"
    (ansi_16 log)

let test_ansi_16_typed_structure () =
  instant := Observe.Instant.of_epoch_nanoseconds 37_425_612_000_000L;
  let log =
    capture_one Observe.Level.Info
      (Observe.Logs.structured event_t
         (User_login { user_id = 42; method_ = "oauth" }))
  in
  Alcotest.(check string)
    "constructor and fields"
    "\027[90m10:23:45.612\027[0m \027[1;96mINFO\027[0m \
     \027[1;96m[example]\027[0m\n\
    \  \027[90m└─\027[0m \027[1;95mUser_login\027[0m\n\
    \     \027[90m├─\027[0m \027[95muser_id\027[0m\027[90m:\027[0m \
     \027[93m42\027[0m\n\
    \     \027[90m└─\027[0m \027[95mmethod_\027[0m\027[90m:\027[0m \
     \027[92moauth\027[0m"
    (ansi_16 log)

let check_formatter_error expected log =
  Alcotest.(check bool)
    "formatter error" true
    (Observe.Formatter.format
       (Observe.Formatter.readable Observe.Formatter.Plain)
       log
    = Error expected)

let test_projection_failures () =
  instant := Observe.Instant.of_epoch_nanoseconds 0L;
  let invalid_utf8 =
    capture_one Observe.Level.Info (Observe.Logs.text ~tag:"invalid" "\255")
  in
  check_formatter_error Observe.Formatter.Invalid_utf8 invalid_utf8;
  let invalid_typed_utf8 =
    capture_one Observe.Level.Info
      (Observe.Logs.structured Observe.Type.string "\255")
  in
  Alcotest.(check string)
    "Repr preserves invalid bytes as base64"
    "00:00:00.000 INFO [example]\n  └─ base64: /w=="
    (readable invalid_typed_utf8);
  let non_finite =
    capture_one Observe.Level.Info
      (Observe.Logs.free (fun () -> Observe.Value.float nan))
  in
  check_formatter_error Observe.Formatter.Non_finite_float non_finite;
  let open Observe.Type in
  let unsupported =
    partially_abstract ~pp:Structural ~of_string:Structural ~json:Undefined
      ~bin:Structural ~unboxed_bin:Structural ~equal:Structural
      ~compare:Structural ~short_hash:Structural ~pre_hash:Structural int
  in
  let unsupported =
    capture_one Observe.Level.Info (Observe.Logs.structured unsupported 1)
  in
  Alcotest.(check bool)
    "unsupported Repr JSON" true
    (Observe.Formatter.format Observe.Formatter.json unsupported
    = Error Observe.Formatter.Unsupported_value)

let () =
  Alcotest.run "observe-formatter"
    [
      ( "readable text",
        [
          Alcotest.test_case "compact information text" `Quick
            test_compact_information_text;
          Alcotest.test_case "visible levels" `Quick test_visible_text_levels;
          Alcotest.test_case "timestamp boundaries" `Quick
            test_timestamp_boundaries;
          Alcotest.test_case "control escaping" `Quick
            test_text_control_escaping;
        ] );
      ( "readable structure",
        [
          Alcotest.test_case "free-form tree" `Quick test_free_form_tree;
          Alcotest.test_case "typed variant tree" `Quick test_typed_variant_tree;
          Alcotest.test_case "nested values and strings" `Quick
            test_nested_values_and_strings;
          Alcotest.test_case "unambiguous literals" `Quick
            test_unambiguous_literals;
          Alcotest.test_case "empty root structure" `Quick
            test_empty_root_structure;
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
