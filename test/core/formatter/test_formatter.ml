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
type child_event = { result : string } [@@deriving observe]
type reserved_event = { service : string } [@@deriving observe]

let take label values =
  match !values with
  | value :: rest ->
      values := rest;
      value
  | [] -> Alcotest.failf "%s fixture was exhausted" label

let timestamp = ref (Observe.Timestamp.of_unix_ns 37_425_612_000_000L)
let monotonic = ref []
let identities = ref []

let observer =
  let host =
    Test_io.Host.create
      ~now:(fun () -> Ok !timestamp)
      ~monotonic_now:(fun () -> Ok (take "monotonic" monotonic))
      ~next_id:(fun () -> Ok (take "identity" identities))
      ()
  in
  Observer.create host

let capture_outcome level message =
  let config = Test_io.config ~min_level:Observe.Level.Debug "example" in
  match
    Observer.with_capture observer ~config (fun capture ->
        Observe.Logs.log ~level message;
        (Observe.Capture.logs capture, Observe.Capture.diagnostics capture))
  with
  | Ok outcome -> outcome
  | Error Observe.IO_already_registered ->
      Alcotest.fail "I/O implementation unexpectedly conflicted"
  | Error (Observe.Invalid_capacity capacity) ->
      Alcotest.failf "unexpected invalid capacity: %d" capacity

let capture_one level message =
  match capture_outcome level message with
  | [ log ], _ -> log
  | logs, _ ->
      Alcotest.failf "expected one captured log, received %d" (List.length logs)

let untyped = Generated_logging.untyped

type 'a described_event = { value : 'a }

type 'a described_event_builder = {
  typed :
    'a described_event Observe.Schema.patch ->
    'a described_event Observe.Schema.patch;
}

let typed description value (builder : Observe.Logs.builder) =
  let event_t =
    let open Observe.Type in
    record "described_event" (fun value -> { value })
    |+ field "value" description (fun event -> event.value)
    |> sealr
  in
  let schema =
    Observe.Schema.record event_t ~builder:(fun _ -> { typed = Fun.id })
  in
  builder.typed ~using:schema { value }

let format_pretty style log =
  match Observe.Formatter.format (Observe.Formatter.pretty style) log with
  | Ok output -> output
  | Error _ -> Alcotest.fail "pretty formatter rejected a valid log"

let pretty log = format_pretty Observe.Formatter.Plain log
let ansi_16 log = format_pretty Observe.Formatter.Ansi_16 log

let format_json formatter log =
  match Observe.Formatter.format formatter log with
  | Ok output -> output
  | Error _ -> Alcotest.fail "JSON formatter rejected a valid log"

type wide_fixtures = {
  correlated : Observe.Log.t;
  child : Observe.Log.t;
  parent : Observe.Log.t;
}

let wide_fixtures () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  monotonic := [ 0L; 10L; 71_000_010L; 184_000_000L ];
  identities := [ "op_parent"; "op_child" ];
  let outcome =
    Observer.with_capture observer
      ~config:(Test_io.config ~min_level:Observe.Level.Debug "example")
      (fun capture ->
        let parent = Observe.Logs.create ~name:"checkout" () in
        let child =
          Observe.Logs.create_typed ~parent ~name:"charge-card"
            ~using:child_event_schema ()
        in
        Observe.Logs.info ~operation:parent
          (Test_io.text ~tag:"inventory" "waiting");
        Observe.Logs.annotate parent ~level:Observe.Level.Warn (fun () ->
            "inventory delayed");
        Observe.Logs.set parent (fun m ->
            let open Observe.Logs in
            m.untyped
            |+ m.field "cart_id" Observe.Type.string "cart-1"
            |+ m.field "consumer_operation" Observe.Type.string "reserve"
            |+ m.field "consumer_service" Observe.Type.string "inventory"
            |> m.seal);
        Observe.Logs.set child (fun m ->
            m.typed (child_event_patch ~result:"authorized" ()));
        Observe.Logs.emit child;
        Observe.Logs.emit parent;
        Observe.Capture.logs capture)
  in
  match outcome with
  | Ok [ correlated; child; parent ] -> { correlated; child; parent }
  | Ok logs ->
      Alcotest.failf "expected three wide fixtures, received %d"
        (List.length logs)
  | Error Observe.IO_already_registered ->
      Alcotest.fail "I/O implementation unexpectedly conflicted"
  | Error (Observe.Invalid_capacity capacity) ->
      Alcotest.failf "unexpected invalid capacity: %d" capacity

let strip_ansi value =
  let buffer = Buffer.create (String.length value) in
  let length = String.length value in
  let rec copy index =
    if index = length then Buffer.contents buffer
    else if
      index + 1 < length && value.[index] = '\027' && value.[index + 1] = '['
    then skip (index + 2)
    else (
      Buffer.add_char buffer value.[index];
      copy (index + 1))
  and skip index =
    if index = length then Buffer.contents buffer
    else if value.[index] = 'm' then copy (index + 1)
    else skip (index + 1)
  in
  copy 0

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
  check_timestamp (-1L) "23:59:59.999";
  timestamp := Observe.Timestamp.of_unix_ns (-1L);
  let log =
    capture_one Observe.Level.Info (Test_io.text ~tag:"time" "message")
  in
  Alcotest.(check string)
    "RFC 3339 preserves a pre-epoch nanosecond"
    "{\"service\":\"example\",\"timestamp\":\"1969-12-31T23:59:59.999999999Z\",\"level\":\"info\",\"tag\":\"time\",\"message\":\"message\"}"
    (format_json Observe.Formatter.json log)

let check_rfc3339_timestamp nanoseconds expected =
  timestamp := Observe.Timestamp.of_unix_ns nanoseconds;
  let log =
    capture_one Observe.Level.Info (Test_io.text ~tag:"time" "message")
  in
  let expected =
    "{\"service\":\"example\",\"timestamp\":\""
    ^ expected
    ^ "\",\"level\":\"info\",\"tag\":\"time\",\"message\":\"message\"}"
  in
  Alcotest.(check string)
    "exact RFC 3339 timestamp" expected
    (format_json Observe.Formatter.json log)

let test_rfc3339_calendar_boundaries () =
  List.iter
    (fun (nanoseconds, expected) ->
      check_rfc3339_timestamp nanoseconds expected)
    [
      (Int64.min_int, "1677-09-21T00:12:43.145224192Z");
      (-2_203_977_600_000_000_000L, "1900-02-28T00:00:00.000000000Z");
      (951_782_400_000_000_000L, "2000-02-29T00:00:00.000000000Z");
      (4_107_542_400_000_000_000L, "2100-03-01T00:00:00.000000000Z");
      (Int64.max_int, "2262-04-11T23:47:16.854775807Z");
    ]

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
    \  └─ value\n\
    \     └─ User_login\n\
    \        ├─ user_id: 42\n\
    \        └─ method_: \"oauth\""
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
    \  └─ value\n\
    \     └─ Session_started\n\
    \        ├─ user_id: 42\n\
    \        ├─ method_: \"oauth\"\n\
    \        ├─ label: \"Granted\"\n\
    \        ├─ access: Granted\n\
    \        ├─ deployment: `Development\n\
    \        ├─ roles: [\"admin\", \"billing\"]\n\
    \        ├─ remembered: true\n\
    \        └─ provider: \"github\""
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
    \  └─ value\n\
    \     └─ Branch\n\
    \        ├─ [0]\n\
    \        │  └─ Leaf: \"one\"\n\
    \        └─ [1]\n\
    \           └─ Branch\n\
    \              └─ [0]\n\
    \                 └─ Leaf: \"two\""
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
    "10:23:45.612 INFO [example]\n  └─ value: Bad\\u001b[31mName" (pretty log)

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
    "10:23:45.612 INFO [example]\n  └─ value: [`Development, `Staging]";
  let queue = Queue.create () in
  Queue.add Granted queue;
  Queue.add Denied queue;
  check "queue renders directly"
    (Observe.Type.queue access_t)
    queue "10:23:45.612 INFO [example]\n  └─ value: [Granted, Denied]";
  let stack = Stack.create () in
  Stack.push `Production stack;
  check "stack renders directly"
    (Observe.Type.stack deployment_t)
    stack "10:23:45.612 INFO [example]\n  └─ value: [`Production]";
  let table = Hashtbl.create 1 in
  Hashtbl.add table "permission" Granted;
  check "hash table renders directly"
    (Observe.Type.hashtbl Observe.Type.string access_t)
    table "10:23:45.612 INFO [example]\n  └─ value: [[\"permission\", Granted]]"

let canonical_failure diagnostics =
  List.exists
    (fun (entry : Observe.Diagnostics.entry) ->
      entry.kind = Observe.Diagnostics.Canonical_freeze_failed
      && entry.count = 1)
    diagnostics

let check_withheld name message =
  let logs, diagnostics = capture_outcome Observe.Level.Info message in
  Alcotest.(check int) (name ^ " output") 0 (List.length logs);
  Alcotest.(check bool)
    (name ^ " diagnostic") true
    (canonical_failure diagnostics)

let test_canonical_failures_are_withheld () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  check_withheld "invalid text" (Test_io.text ~tag:"invalid" "\255");
  check_withheld "invalid structured UTF-8"
    (untyped (fun () ->
         Observe.Value.object_ [ ("value", Observe.Value.string "\255") ]));
  check_withheld "non-finite float"
    (untyped (fun () ->
         Observe.Value.object_ [ ("value", Observe.Value.float nan) ]));
  check_withheld "reserved root metadata"
    (untyped (fun () ->
         Observe.Value.object_
           [ ("service", Observe.Value.string "consumer-value") ]));
  check_withheld "reserved typed root metadata" (fun builder ->
      builder.typed ~using:reserved_event_schema { service = "consumer-value" });
  ()

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
    \  \027[90m└─\027[0m \027[95mvalue\027[0m\n\
    \     \027[90m└─\027[0m \027[1;95mUser_login\027[0m\n\
    \        \027[90m├─\027[0m \027[95muser_id\027[0m\027[90m:\027[0m \
     \027[93m42\027[0m\n\
    \        \027[90m└─\027[0m \027[95mmethod_\027[0m\027[90m:\027[0m \
     \027[92m\"oauth\"\027[0m"
    (ansi_16 log)

let test_wide_pretty_layout () =
  let fixtures = wide_fixtures () in
  Alcotest.(check string)
    "correlated point"
    "10:23:45.612 INFO [inventory] waiting\n\
    \  └─ operation: checkout (op_parent)"
    (pretty fixtures.correlated);
  Alcotest.(check string)
    "child wide"
    "10:23:45.612 INFO [charge-card] 71ms\n\
    \  ├─ id: \"op_child\"\n\
    \  ├─ parent: checkout (op_parent)\n\
    \  └─ result: \"authorized\""
    (pretty fixtures.child);
  Alcotest.(check string)
    "parent wide"
    "10:23:45.612 WARN [checkout] 184ms\n\
    \  ├─ id: \"op_parent\"\n\
    \  ├─ cart_id: \"cart-1\"\n\
    \  ├─ consumer_operation: \"reserve\"\n\
    \  ├─ consumer_service: \"inventory\"\n\
    \  └─ logs\n\
    \     └─ [0]\n\
    \        ├─ timestamp: \"1970-01-01T10:23:45.612000000Z\"\n\
    \        ├─ level: \"warn\"\n\
    \        └─ message: \"inventory delayed\""
    (pretty fixtures.parent);
  List.iter
    (fun style ->
      Alcotest.(check string)
        "style changes color only" (pretty fixtures.child)
        (format_pretty style fixtures.child |> strip_ansi))
    [
      Observe.Formatter.Ansi_16;
      Observe.Formatter.Ansi_256;
      Observe.Formatter.Truecolor;
    ]

let test_wide_json_events () =
  let fixtures = wide_fixtures () in
  let correlated =
    "{\"service\":\"example\",\"timestamp\":\"1970-01-01T10:23:45.612000000Z\",\"level\":\"info\",\"operation\":\"checkout\",\"operation_id\":\"op_parent\",\"tag\":\"inventory\",\"message\":\"waiting\"}"
  in
  let child =
    "{\"service\":\"example\",\"timestamp\":\"1970-01-01T10:23:45.612000000Z\",\"level\":\"info\",\"operation\":\"charge-card\",\"operation_id\":\"op_child\",\"parent_operation\":\"checkout\",\"parent_operation_id\":\"op_parent\",\"duration_ms\":71,\"result\":\"authorized\"}"
  in
  let parent =
    "{\"service\":\"example\",\"timestamp\":\"1970-01-01T10:23:45.612000000Z\",\"level\":\"warn\",\"operation\":\"checkout\",\"operation_id\":\"op_parent\",\"duration_ms\":184,\"cart_id\":\"cart-1\",\"consumer_operation\":\"reserve\",\"consumer_service\":\"inventory\",\"logs\":[{\"timestamp\":\"1970-01-01T10:23:45.612000000Z\",\"level\":\"warn\",\"message\":\"inventory \
     delayed\"}]}"
  in
  List.iter
    (fun (name, log, expected) ->
      let json = format_json Observe.Formatter.json log in
      let ndjson = format_json Observe.Formatter.ndjson log in
      Alcotest.(check string) name expected json;
      Alcotest.(check string) (name ^ " NDJSON") (json ^ "\n") ndjson)
    [
      ("correlated point", fixtures.correlated, correlated);
      ("child wide", fixtures.child, child);
      ("parent flat event", fixtures.parent, parent);
    ]

let test_wide_metadata_escaping () =
  timestamp := Observe.Timestamp.of_unix_ns 37_425_612_000_000L;
  monotonic := [ 0L; 1_000L ];
  identities := [ "op\nid" ];
  let outcome =
    Observer.with_capture observer
      ~config:(Test_io.config ~min_level:Observe.Level.Debug "example")
      (fun capture ->
        let wide = Observe.Logs.create ~name:"checkout\027[31m" () in
        Observe.Logs.emit wide;
        Observe.Capture.logs capture)
  in
  let log =
    match outcome with
    | Ok [ log ] -> log
    | Ok logs ->
        Alcotest.failf "expected one escaped fixture, received %d"
          (List.length logs)
    | Error Observe.IO_already_registered ->
        Alcotest.fail "I/O implementation unexpectedly conflicted"
    | Error (Observe.Invalid_capacity capacity) ->
        Alcotest.failf "unexpected invalid capacity: %d" capacity
  in
  Alcotest.(check string)
    "pretty metadata is terminal safe"
    "10:23:45.612 INFO [checkout\\u001b[31m] 1us\n  └─ id: \"op\\nid\""
    (pretty log);
  Alcotest.(check string)
    "JSON metadata is escaped"
    "{\"service\":\"example\",\"timestamp\":\"1970-01-01T10:23:45.612000000Z\",\"level\":\"info\",\"operation\":\"checkout\\u001b[31m\",\"operation_id\":\"op\\nid\",\"duration_ms\":0.001}"
    (format_json Observe.Formatter.json log)

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
          Alcotest.test_case "RFC 3339 calendar boundaries" `Quick
            test_rfc3339_calendar_boundaries;
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
        ] );
      ( "ANSI styling",
        [
          Alcotest.test_case "tagged text" `Quick test_ansi_16_text;
          Alcotest.test_case "capability depths" `Quick test_color_depths;
          Alcotest.test_case "structured tree" `Quick test_ansi_16_structure;
          Alcotest.test_case "typed structured tree" `Quick
            test_ansi_16_typed_structure;
        ] );
      ( "canonical boundary",
        [
          Alcotest.test_case "unsafe values are withheld" `Quick
            test_canonical_failures_are_withheld;
        ] );
      ( "wide observations",
        [
          Alcotest.test_case "pretty layout and style parity" `Quick
            test_wide_pretty_layout;
          Alcotest.test_case "JSON and NDJSON events" `Quick
            test_wide_json_events;
          Alcotest.test_case "metadata escaping" `Quick
            test_wide_metadata_escaping;
        ] );
    ]
