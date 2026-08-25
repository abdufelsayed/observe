let check_level expected level =
  Alcotest.(check string)
    "stable level spelling" expected
    (Observe.Level.to_string level);
  Alcotest.(check string)
    "level printer" expected
    (Format.asprintf "%a" Observe.Level.pp level)

let test_level_contract () =
  let levels =
    [
      (Observe.Level.Debug, "debug");
      (Observe.Level.Info, "info");
      (Observe.Level.Warn, "warn");
      (Observe.Level.Error, "error");
    ]
  in
  List.iter (fun (level, spelling) -> check_level spelling level) levels;
  let rec ordered = function
    | [] | [ _ ] -> true
    | (left, _) :: ((right, _) :: _ as rest) ->
        Observe.Level.compare left right < 0 && ordered rest
  in
  Alcotest.(check bool) "documented admission order" true (ordered levels)

let test_timestamp_contract () =
  List.iter
    (fun nanoseconds ->
      let timestamp = Observe.Timestamp.of_unix_ns nanoseconds in
      Alcotest.(check int64)
        "Unix nanoseconds round-trip" nanoseconds
        (Observe.Timestamp.to_unix_ns timestamp);
      Alcotest.(check string)
        "timestamp printer"
        (Int64.to_string nanoseconds)
        (Format.asprintf "%a" Observe.Timestamp.pp timestamp))
    [ Int64.min_int; -1L; 0L; 1L; Int64.max_int ]

let invalid byte = String.make 1 (Char.chr byte)

type manual_access = Granted | Denied of string

let manual_access_t =
  let open Observe.Type in
  variant "manual_access" (fun granted denied -> function
    | Granted -> granted
    | Denied reason -> denied reason)
  |~ case0
       ~is:(function Granted -> true | Denied _ -> false)
       "Granted" Granted
  |~ case1
       ~project:(function Denied reason -> Some reason | Granted -> None)
       "Denied" string
       (fun reason -> Denied reason)
  |> sealv

type manual_node = Leaf of string | Branch of manual_node list

let manual_node_t =
  Observe.Type.mu (fun node_t ->
      let open Observe.Type in
      variant "manual_node" (fun leaf branch -> function
        | Leaf value -> leaf value
        | Branch children -> branch children)
      |~ case1
           ~project:(function Leaf value -> Some value | Branch _ -> None)
           "Leaf" string
           (fun value -> Leaf value)
      |~ case1
           ~project:(function Branch values -> Some values | Leaf _ -> None)
           "Branch" (list node_t)
           (fun values -> Branch values)
      |> sealv)

let check_config_error ~field ~problem = function
  | Ok _ -> Alcotest.fail "invalid configuration was accepted"
  | Error error ->
      Alcotest.(check bool)
        "invalid field" true
        (error.Observe.Config.field = field);
      Alcotest.(check bool)
        "invalid problem" true
        (error.Observe.Config.problem = problem)

let test_config_contract () =
  let defaults = Observe.Config.create_exn ~service:"observe-test" () in
  Alcotest.(check string)
    "service" "observe-test"
    (Observe.Config.service defaults);
  Alcotest.(check (option string))
    "environment" None
    (Observe.Config.environment defaults);
  Alcotest.(check (option string))
    "version" None
    (Observe.Config.version defaults);
  Alcotest.(check bool) "enabled" true (Observe.Config.enabled defaults);
  Alcotest.(check bool)
    "automatic console" true
    (Observe.Config.console defaults = Observe.Config.Auto);
  Alcotest.(check bool)
    "minimum info" true
    (Observe.Level.equal Observe.Level.Info (Observe.Config.min_level defaults));
  Alcotest.(check int)
    "no drains" 0
    (List.length (Observe.Config.drains defaults));
  let explicit =
    Observe.Config.create_exn ~service:"api" ~environment:"test" ~version:"1"
      ~enabled:false ~console:Observe.Config.Silent
      ~min_level:Observe.Level.Error ()
  in
  Alcotest.(check (option string))
    "explicit environment" (Some "test")
    (Observe.Config.environment explicit);
  Alcotest.(check (option string))
    "explicit version" (Some "1")
    (Observe.Config.version explicit);
  Alcotest.(check bool)
    "explicit enabled" false
    (Observe.Config.enabled explicit);
  Alcotest.(check bool)
    "explicit silent" true
    (Observe.Config.console explicit = Observe.Config.Silent);
  Alcotest.(check bool)
    "explicit minimum level" true
    (Observe.Level.equal Observe.Level.Error
       (Observe.Config.min_level explicit));
  let development =
    Observe.Config.create_exn ~service:"api" ~environment:"development" ()
  in
  Alcotest.(check bool)
    "development remains automatic" true
    (Observe.Config.console development = Observe.Config.Auto);
  let dev = Observe.Config.create_exn ~service:"api" ~environment:"dev" () in
  Alcotest.(check bool)
    "dev remains automatic" true
    (Observe.Config.console dev = Observe.Config.Auto);
  let production =
    Observe.Config.create_exn ~service:"api" ~environment:"production" ()
  in
  Alcotest.(check bool)
    "production remains automatic" true
    (Observe.Config.console production = Observe.Config.Auto);
  let staging =
    Observe.Config.create_exn ~service:"api" ~environment:"staging" ()
  in
  Alcotest.(check bool)
    "staging remains automatic" true
    (Observe.Config.console staging = Observe.Config.Auto);
  let development_override =
    Observe.Config.create_exn ~service:"api" ~environment:"development"
      ~console:Observe.Config.Ndjson ()
  in
  Alcotest.(check bool)
    "development override" true
    (Observe.Config.console development_override = Observe.Config.Ndjson);
  let production_override =
    Observe.Config.create_exn ~service:"api" ~environment:"production"
      ~console:Observe.Config.Pretty ()
  in
  Alcotest.(check bool)
    "production override" true
    (Observe.Config.console production_override = Observe.Config.Pretty);
  check_config_error ~field:Observe.Config.Service ~problem:Observe.Config.Empty
    (Observe.Config.create ~service:"" ());
  check_config_error ~field:Observe.Config.Environment
    ~problem:Observe.Config.Invalid_utf8
    (Observe.Config.create ~service:"service" ~environment:(invalid 0xff) ());
  check_config_error ~field:Observe.Config.Version
    ~problem:Observe.Config.Invalid_utf8
    (Observe.Config.create ~service:"service" ~version:(invalid 0x80) ())

let test_config_exception_and_printer () =
  let expected =
    {
      Observe.Config.field = Observe.Config.Service;
      problem = Observe.Config.Empty;
    }
  in
  Alcotest.check_raises "create_exn preserves structured error"
    (Observe.Config.Invalid_configuration expected) (fun () ->
      ignore (Observe.Config.create_exn ~service:"" ()));
  Alcotest.(check string)
    "configuration error text" "service must not be empty"
    (Format.asprintf "%a" Observe.Config.pp_error expected)

let test_value_contract () =
  let value =
    Observe.Value.object_
      [
        ("null", Observe.Value.null);
        ("bool", Observe.Value.bool true);
        ("int", Observe.Value.int 7);
        ("float", Observe.Value.float 1.5);
        ("string", Observe.Value.string "hello");
        ("none", Observe.Value.option None);
        ("some", Observe.Value.option (Some (Observe.Value.int 9)));
        ("list", Observe.Value.list [ Observe.Value.int 1; Observe.Value.int 2 ]);
        ("embedded", Observe.Value.embed Observe.Type.int 11);
      ]
  in
  let rendered = Observe.Value.to_string value in
  Alcotest.(check bool) "pretty object" true (String.length rendered > 0);
  Alcotest.(check string)
    "pp and to_string agree" rendered
    (Format.asprintf "%a" Observe.Value.pp value);
  (match value with
  | Observe.Value.Object fields ->
      Alcotest.(check int)
        "private structure is inspectable" 9 (List.length fields)
  | Observe.Value.Null | Observe.Value.Bool _ | Observe.Value.Int _
  | Observe.Value.Float _ | Observe.Value.String _ | Observe.Value.List _
  | Observe.Value.Embedded _ ->
      Alcotest.fail "object constructor was not observable");
  Alcotest.(check bool)
    "direct JSON is public" true
    (match Observe.Value.to_json_string value with
    | Ok encoded -> String.length encoded > 2 && encoded.[0] = '{'
    | Error _ -> false)

let test_type_interoperability () =
  Alcotest.(check string)
    "Observe description exposes Repr" "42"
    (Repr.to_json_string ~minify:true (Observe.Type.repr Observe.Type.int) 42)

let test_manual_description_json () =
  let check description value =
    let direct = Observe.Type.to_json_string description value in
    let repr =
      Repr.to_json_string ~minify:true (Observe.Type.repr description) value
    in
    Alcotest.(check string) "direct JSON agrees with Repr" repr direct;
    match Observe.Type.of_json_string description direct with
    | Ok decoded -> decoded
    | Error (`Msg message) -> Alcotest.failf "JSON did not decode: %s" message
  in
  Alcotest.(check bool)
    "manual nullary variant" true
    (check manual_access_t Granted = Granted);
  Alcotest.(check bool)
    "manual payload variant" true
    (check manual_access_t (Denied "policy") = Denied "policy");
  let node = Branch [ Leaf "one"; Branch [ Leaf "two" ] ] in
  Alcotest.(check bool)
    "manual recursive variant" true
    (check manual_node_t node = node)

let test_large_list_json_is_stack_safe () =
  let values = List.init 100_000 Fun.id in
  let encoded =
    Observe.Type.to_json_string (Observe.Type.list Observe.Type.int) values
  in
  Alcotest.(check char) "opening bracket" '[' encoded.[0];
  Alcotest.(check char)
    "closing bracket" ']'
    encoded.[String.length encoded - 1]

let test_asynchronous_drain_failure_diagnostic () =
  let count () =
    List.fold_left
      (fun total (entry : Observe.Diagnostics.entry) ->
        if entry.kind = Observe.Diagnostics.Drain_delivery_failed then
          total + entry.count
        else total)
      0
      (Observe.Diagnostics.snapshot ())
  in
  let before = count () in
  let first = Observe.Drain.create (fun _ -> Observe.Drain.Accepted) in
  let second = Observe.Drain.create (fun _ -> Observe.Drain.Accepted) in
  Observe.Drain.Integration.report_failure first;
  Observe.Drain.Integration.report_failure first;
  Observe.Drain.Integration.report_failure second;
  Alcotest.(check int)
    "one bounded asynchronous failure for each owning drain" (before + 2)
    (count ())

let () =
  Alcotest.run "observe-public-contracts"
    [
      ( "unit:observe:level-timestamp",
        [
          Alcotest.test_case "levels" `Quick test_level_contract;
          Alcotest.test_case "timestamps" `Quick test_timestamp_contract;
        ] );
      ( "unit:observe:configuration",
        [
          Alcotest.test_case "creation" `Quick test_config_contract;
          Alcotest.test_case "exception and printer" `Quick
            test_config_exception_and_printer;
        ] );
      ( "unit:observe:value",
        [
          Alcotest.test_case "public constructors" `Quick test_value_contract;
          Alcotest.test_case "Repr interoperability" `Quick
            test_type_interoperability;
          Alcotest.test_case "manual descriptions compile JSON" `Quick
            test_manual_description_json;
          Alcotest.test_case "large-list JSON is stack safe" `Quick
            test_large_list_json_is_stack_safe;
        ] );
      ( "unit:observe:drain",
        [
          Alcotest.test_case "asynchronous failure signal" `Quick
            test_asynchronous_drain_failure_diagnostic;
        ] );
    ]
