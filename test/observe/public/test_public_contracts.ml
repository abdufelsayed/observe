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

let test_instant_contract () =
  List.iter
    (fun nanoseconds ->
      let instant = Observe.Instant.of_epoch_nanoseconds nanoseconds in
      Alcotest.(check int64)
        "epoch nanoseconds round-trip" nanoseconds
        (Observe.Instant.to_epoch_nanoseconds instant);
      Alcotest.(check string)
        "instant printer"
        (Int64.to_string nanoseconds)
        (Format.asprintf "%a" Observe.Instant.pp instant))
    [ Int64.min_int; -1L; 0L; 1L; Int64.max_int ]

let invalid byte = String.make 1 (Char.chr byte)

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
  Alcotest.(check bool) "pretty" true (Observe.Config.pretty defaults);
  Alcotest.(check bool) "silent" false (Observe.Config.silent defaults);
  Alcotest.(check bool)
    "minimum info" true
    (Observe.Level.equal Observe.Level.Info (Observe.Config.min_level defaults));
  Alcotest.(check int)
    "no drains" 0
    (List.length (Observe.Config.drains defaults));
  let explicit =
    Observe.Config.create_exn ~service:"api" ~environment:"test" ~version:"1"
      ~enabled:false ~pretty:false ~silent:true ~min_level:Observe.Level.Error
      ()
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
  Alcotest.(check bool) "explicit pretty" false (Observe.Config.pretty explicit);
  Alcotest.(check bool) "explicit silent" true (Observe.Config.silent explicit);
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
  Alcotest.(check bool) "readable object" true (String.length rendered > 0);
  Alcotest.(check string)
    "pp and to_string agree" rendered
    (Format.asprintf "%a" Observe.Value.pp value)

let () =
  Alcotest.run "observe-public-contracts"
    [
      ( "unit:observe:level-instant",
        [
          Alcotest.test_case "levels" `Quick test_level_contract;
          Alcotest.test_case "instants" `Quick test_instant_contract;
        ] );
      ( "unit:observe:configuration",
        [
          Alcotest.test_case "creation" `Quick test_config_contract;
          Alcotest.test_case "exception and printer" `Quick
            test_config_exception_and_printer;
        ] );
      ( "unit:observe:value",
        [ Alcotest.test_case "public constructors" `Quick test_value_contract ]
      );
    ]
