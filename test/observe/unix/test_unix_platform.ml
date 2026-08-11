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

let instant_nanoseconds parts =
  match Clock.instant parts with
  | Ok instant -> Observe.Instant.to_epoch_nanoseconds instant
  | Error Observe.Platform.Unavailable -> Alcotest.fail "instant unavailable"

let test_clock_conversion () =
  Alcotest.(check int64) "epoch" 0L (instant_nanoseconds (0, 0L));
  Alcotest.(check int64)
    "day and picoseconds" 86_400_000_000_001L
    (instant_nanoseconds (1, 1_234L));
  let overflow_days =
    Int64.div Int64.max_int 86_400_000_000_000L |> Int64.succ |> Int64.to_int
  in
  Alcotest.(check bool)
    "out of Instant range" true
    (Clock.instant (overflow_days, 0L) = Error Observe.Platform.Unavailable)

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

let test_terminal_bytes () =
  let input, output = Unix.pipe () in
  let saved = Unix.dup Unix.stderr in
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 saved Unix.stderr;
      Unix.close saved;
      Unix.close input)
    (fun () ->
      Unix.dup2 output Unix.stderr;
      Unix.close output;
      let acceptance =
        Observe_unix.Platform.write_terminal () "exact record\n"
      in
      Unix.dup2 saved Unix.stderr;
      Alcotest.(check bool)
        "accepted" true
        (acceptance = Observe.Platform.Accepted);
      Alcotest.(check string)
        "bytes unchanged" "exact record\n" (read_all input))

let test_partial_and_interrupted_writes () =
  let calls = ref 0 in
  let received = Buffer.create 8 in
  let write _ value offset remaining =
    incr calls;
    if !calls = 1 then raise (Unix.Unix_error (Unix.EINTR, "write", ""));
    let count = min 2 remaining in
    Buffer.add_substring received value offset count;
    count
  in
  Write.all ~write Unix.stderr "abcdef";
  Alcotest.(check string) "all bytes" "abcdef" (Buffer.contents received);
  Alcotest.(check int) "interruption plus partial writes" 4 !calls

let () =
  Alcotest.run "observe-unix"
    [
      ( "platform",
        [
          Alcotest.test_case "wall clock" `Quick test_clock;
          Alcotest.test_case "clock conversion" `Quick test_clock_conversion;
          Alcotest.test_case "terminal bytes" `Quick test_terminal_bytes;
          Alcotest.test_case "partial and interrupted writes" `Quick
            test_partial_and_interrupted_writes;
        ] );
    ]
