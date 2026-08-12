let test_clock () =
  let before = Unix.gettimeofday () in
  let instant =
    match Observe_unix.Platform.now () with
    | Ok instant -> Observe.Instant.to_epoch_nanoseconds instant
    | Error Observe.Platform.Unavailable -> Alcotest.fail "OS clock unavailable"
  in
  let after = Unix.gettimeofday () in
  let seconds = Int64.to_float instant /. 1_000_000_000.0 in
  Alcotest.(check bool)
    "not before surrounding clock" true
    (seconds >= before -. 0.001);
  Alcotest.(check bool)
    "not after surrounding clock" true
    (seconds <= after +. 0.001)

let read_all descriptor =
  let buffer = Bytes.create 64 in
  let output = Buffer.create 64 in
  let rec loop () =
    match Unix.read descriptor buffer 0 (Bytes.length buffer) with
    | 0 -> Buffer.contents output
    | count ->
        Buffer.add_subbytes output buffer 0 count;
        loop ()
  in
  loop ()

let test_console_bytes () =
  let input, output = Unix.pipe () in
  let saved = Unix.dup Unix.stderr in
  let style, acceptance, bytes =
    Fun.protect
      ~finally:(fun () ->
        Unix.dup2 saved Unix.stderr;
        Unix.close saved;
        Unix.close input)
      (fun () ->
        Unix.dup2 output Unix.stderr;
        Unix.close output;
        let style = Observe_unix.Platform.console_style () in
        let acceptance =
          Observe_unix.Platform.write_console () "exact record\n"
        in
        Unix.dup2 saved Unix.stderr;
        (style, acceptance, read_all input))
  in
  Alcotest.(check bool)
    "redirected output is plain" true
    (style = Observe.Formatter.Plain);
  Alcotest.(check bool) "accepted" true (acceptance = Observe.Platform.Accepted);
  Alcotest.(check string) "bytes unchanged" "exact record\n" bytes

let () =
  Alcotest.run "observe-unix"
    [
      ( "platform",
        [
          Alcotest.test_case "wall clock" `Quick test_clock;
          Alcotest.test_case "console bytes" `Quick test_console_bytes;
        ] );
    ]
