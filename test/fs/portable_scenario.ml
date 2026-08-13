module IO = struct
  include Observe_fs_test_support.Fs_fixture.Platform

  type 'a t = 'a Lwt.t

  let return = Lwt.return
  let bind = Lwt.bind
  let catch = Lwt.catch
  let async = Lwt.async
end

module Delivery = Observe_fs.Make (IO)
module Observer = Observe.Make (Observe_lwt.IO)

let fail format = Format.kasprintf failwith format

let check condition format =
  Format.kasprintf
    (fun message -> if not condition then failwith message)
    format

let contains value fragment =
  let value_length = String.length value in
  let fragment_length = String.length fragment in
  let rec loop offset =
    offset + fragment_length <= value_length
    && (String.sub value offset fragment_length = fragment || loop (offset + 1))
  in
  fragment_length = 0 || loop 0

let line_count value =
  String.fold_left
    (fun count character -> if character = '\n' then count + 1 else count)
    0 value

let diagnostic_count kind =
  List.fold_left
    (fun total (entry : Observe.Diagnostics.entry) ->
      if entry.kind = kind then total + entry.count else total)
    0
    (Observe.Diagnostics.snapshot ())

let install ?(extra_drains = []) timestamps drain =
  let timestamps = ref timestamps in
  let clock () =
    match !timestamps with
    | timestamp :: rest ->
        timestamps := rest;
        Ok (Observe.Timestamp.of_unix_ns timestamp)
    | [] -> fail "test clock exhausted"
  in
  let io =
    Observe_lwt.create ~clock
      ~console_style:(fun () -> Observe.Formatter.Plain)
      ~write_console:(fun _ -> Observe.IO.Rejected)
      ~can_lookup_context:(fun () -> true)
      ()
  in
  let observer = Observer.create io in
  let config =
    Observe.Config.create_exn ~service:"fs-test" ~console:Observe.Config.Silent
      ~drains:(drain :: extra_drains) ()
  in
  Observer.init_exn observer config

let create ?capacity () =
  Observe_fs_test_support.Fs_fixture.reset ();
  match Lwt_main.run (Delivery.create ~path:"/logs" ?capacity ()) with
  | Ok worker -> worker
  | Error error -> fail "create failed: %a" Delivery.pp_error error

let emit tag message = Observe.Logs.info (Observe.Logs.text ~tag message)

let daily () =
  let worker = create () in
  install [ -1L; 0L; 86_400_000_000_000L; 0L ] (Delivery.drain worker);
  emit "before" "before epoch";
  emit "epoch" "epoch";
  emit "tomorrow" "tomorrow";
  emit "back" "back";
  check
    (Observe_fs_test_support.Fs_fixture.paths ()
    = [
        "/logs/1969-12-31.jsonl";
        "/logs/1970-01-01.jsonl";
        "/logs/1970-01-02.jsonl";
      ])
    "unexpected daily paths";
  Lwt_main.run (Delivery.flush worker) |> Result.get_ok;
  let epoch =
    Observe_fs_test_support.Fs_fixture.contents "/logs/1970-01-01.jsonl"
  in
  check (line_count epoch = 2) "day switch-back lost a record: %S" epoch;
  check (contains epoch "\"tag\":\"epoch\"") "epoch record missing";
  check (contains epoch "\"tag\":\"back\"") "switch-back record missing";
  Lwt_main.run (Delivery.shutdown worker) |> Result.get_ok

let append_and_partial () =
  Observe_fs_test_support.Fs_fixture.reset ();
  Observe_fs_test_support.Fs_fixture.seed "/logs/1970-01-01.jsonl"
    "{\"existing\":true}\n";
  Observe_fs_test_support.Fs_fixture.set_max_write 3;
  let worker =
    Lwt_main.run (Delivery.create ~path:"/logs" ()) |> Result.get_ok
  in
  install [ 0L ] (Delivery.drain worker);
  emit "partial" "complete me";
  Lwt_main.run (Delivery.shutdown worker) |> Result.get_ok;
  let output =
    Observe_fs_test_support.Fs_fixture.contents "/logs/1970-01-01.jsonl"
  in
  check (contains output "{\"existing\":true}\n") "existing data changed";
  check
    (contains output "\"message\":\"complete me\"")
    "partial writes lost bytes: %S" output;
  check (line_count output = 2) "unexpected appended record count: %S" output

type sample = { mutable value : string }

let sample_t =
  let open Observe.Type in
  record "sample" (fun value -> { value })
  |+ field "value" string (fun sample -> sample.value)
  |> sealr

let mutation () =
  let worker = create () in
  install [ 0L ] (Delivery.drain worker);
  let sample = { value = "before" } in
  Observe.Logs.info (Observe.Logs.structured sample_t sample);
  sample.value <- "after";
  Lwt_main.run (Delivery.shutdown worker) |> Result.get_ok;
  let output =
    Observe_fs_test_support.Fs_fixture.contents "/logs/1970-01-01.jsonl"
  in
  check
    (contains output "\"value\":\"before\"")
    "queue retained mutable consumer data: %S" output;
  check (not (contains output "after")) "mutation changed queued output"

let projection () =
  let worker = create () in
  let expected = Buffer.create 256 in
  let witness =
    Observe.Drain.create (fun log ->
        match Observe.Formatter.format Observe.Formatter.ndjson log with
        | Error _ -> Observe.Drain.Rejected
        | Ok bytes ->
            Buffer.add_string expected bytes;
            Observe.Drain.Accepted)
  in
  install ~extra_drains:[ witness ] [ 0L; 1L; 2L ] (Delivery.drain worker);
  emit "text" "text payload";
  Observe.Logs.info
    (Observe.Logs.free
       (Observe.Value.object_
          [
            ("action", Observe.Value.string "free");
            ("count", Observe.Value.int 2);
          ]));
  Observe.Logs.info
    (Observe.Logs.structured sample_t { value = "typed payload" });
  Lwt_main.run (Delivery.shutdown worker) |> Result.get_ok;
  let actual =
    Observe_fs_test_support.Fs_fixture.contents "/logs/1970-01-01.jsonl"
  in
  check
    (String.equal actual (Buffer.contents expected))
    "filesystem changed semantic NDJSON:\nexpected %S\nactual   %S"
    (Buffer.contents expected) actual

let capacity () =
  let worker = create ~capacity:1 () in
  install [ 0L; 1L; 2L ] (Delivery.drain worker);
  let release = Observe_fs_test_support.Fs_fixture.block_writes () in
  let before = diagnostic_count Observe.Diagnostics.Drain_rejected in
  emit "one" "one";
  Lwt_main.run (Lwt.pause ());
  emit "two" "two";
  emit "three" "three";
  check
    (diagnostic_count Observe.Diagnostics.Drain_rejected = before + 1)
    "full queue did not reject exactly the newest record";
  release ();
  Lwt_main.run (Delivery.shutdown worker) |> Result.get_ok;
  let output =
    Observe_fs_test_support.Fs_fixture.contents "/logs/1970-01-01.jsonl"
  in
  check (line_count output = 2) "capacity changed accepted work: %S" output;
  check (contains output "\"tag\":\"one\"") "first record was lost";
  check (contains output "\"tag\":\"two\"") "queued record was lost";
  check
    (not (contains output "\"tag\":\"three\""))
    "rejected record was written"

let default_capacity () =
  let worker = create () in
  install (List.init 1_026 Int64.of_int) (Delivery.drain worker);
  let release = Observe_fs_test_support.Fs_fixture.block_writes () in
  let rejected_before = diagnostic_count Observe.Diagnostics.Drain_rejected in
  emit "active" "active";
  Lwt_main.run (Lwt.pause ());
  for index = 1 to 1_025 do
    emit "queued" (Int.to_string index)
  done;
  check
    (diagnostic_count Observe.Diagnostics.Drain_rejected = rejected_before + 1)
    "default capacity did not reject exactly its newest overflow";
  release ();
  Lwt_main.run (Delivery.shutdown worker) |> Result.get_ok;
  let output =
    Observe_fs_test_support.Fs_fixture.contents "/logs/1970-01-01.jsonl"
  in
  check (line_count output = 1_025) "default capacity changed accepted work"

let flush_barriers () =
  let worker = create () in
  install [ 0L; 1L ] (Delivery.drain worker);
  emit "first" "first";
  let first = Delivery.flush worker in
  emit "second" "second";
  let second = Delivery.flush worker in
  Lwt_main.run
    (Lwt.bind first (fun first ->
         Lwt.bind second (fun second -> Lwt.return (first, second))))
  |> fun (first, second) ->
  Result.get_ok first;
  Result.get_ok second;
  check
    (Observe_fs_test_support.Fs_fixture.operations ()
    = [ `Write; `Flush; `Write; `Flush ])
    "later acceptance extended an earlier flush barrier";
  Lwt_main.run (Delivery.shutdown worker) |> Result.get_ok

let failure () =
  let worker = create () in
  let independent = ref 0 in
  let independent_drain =
    Observe.Drain.create (fun _ ->
        incr independent;
        Observe.Drain.Accepted)
  in
  install ~extra_drains:[ independent_drain ] [ 0L; 1L ] (Delivery.drain worker);
  let failed_before = diagnostic_count Observe.Diagnostics.Drain_failed in
  let rejected_before = diagnostic_count Observe.Diagnostics.Drain_rejected in
  Observe_fs_test_support.Fs_fixture.fail_next_write ();
  emit "failure" "fail";
  let outcome = Lwt_main.run (Delivery.flush worker) in
  check (Result.is_error outcome) "write failure did not fail flush";
  check
    (diagnostic_count Observe.Diagnostics.Drain_failed = failed_before + 1)
    "asynchronous failure was not diagnosed exactly once";
  emit "later" "reject";
  check
    (diagnostic_count Observe.Diagnostics.Drain_rejected = rejected_before + 1)
    "failed worker accepted a later record";
  check (!independent = 2) "filesystem failure stopped an independent drain"

let expect_failure setup =
  let worker = create () in
  install [ 0L ] (Delivery.drain worker);
  setup ();
  emit "failure" "fail";
  check
    (Result.is_error (Lwt_main.run (Delivery.flush worker)))
    "filesystem failure did not settle flush with an error"

let failure_open () =
  expect_failure Observe_fs_test_support.Fs_fixture.fail_next_open

let failure_zero_progress () =
  expect_failure (fun () -> Observe_fs_test_support.Fs_fixture.set_max_write 0)

let failure_flush () =
  expect_failure Observe_fs_test_support.Fs_fixture.fail_next_flush

let failure_close () =
  let worker = create () in
  install [ 0L ] (Delivery.drain worker);
  emit "failure" "fail";
  Observe_fs_test_support.Fs_fixture.fail_next_close ();
  check
    (Result.is_error (Lwt_main.run (Delivery.shutdown worker)))
    "close failure did not settle shutdown with an error"

let invalid_write_count () =
  let worker = create () in
  install [ 0L ] (Delivery.drain worker);
  Observe_fs_test_support.Fs_fixture.set_max_write (-1);
  emit "invalid" "invalid write count";
  match Lwt_main.run (Delivery.flush worker) with
  | Error (Delivery.Invalid_write_count -1) -> ()
  | Error error -> fail "unexpected failure: %a" Delivery.pp_error error
  | Ok () -> fail "invalid write count was accepted"

let shutdown () =
  let worker = create () in
  install [ 0L; 1L ] (Delivery.drain worker);
  emit "before" "before";
  Lwt_main.run (Delivery.shutdown worker) |> Result.get_ok;
  Lwt_main.run (Delivery.shutdown worker) |> Result.get_ok;
  let rejected_before = diagnostic_count Observe.Diagnostics.Drain_rejected in
  emit "after" "after";
  check
    (diagnostic_count Observe.Diagnostics.Drain_rejected = rejected_before + 1)
    "shutdown worker accepted later output";
  check
    (Observe_fs_test_support.Fs_fixture.close_count () = 1)
    "shutdown did not close exactly once"

let () =
  match Array.to_list Sys.argv with
  | [ _; "daily" ] -> daily ()
  | [ _; "append-and-partial" ] -> append_and_partial ()
  | [ _; "mutation" ] -> mutation ()
  | [ _; "projection" ] -> projection ()
  | [ _; "capacity" ] -> capacity ()
  | [ _; "default-capacity" ] -> default_capacity ()
  | [ _; "flush-barriers" ] -> flush_barriers ()
  | [ _; "failure" ] -> failure ()
  | [ _; "failure-open" ] -> failure_open ()
  | [ _; "failure-zero-progress" ] -> failure_zero_progress ()
  | [ _; "failure-flush" ] -> failure_flush ()
  | [ _; "failure-close" ] -> failure_close ()
  | [ _; "invalid-write-count" ] -> invalid_write_count ()
  | [ _; "shutdown" ] -> shutdown ()
  | _ -> fail "unknown portable filesystem scenario"
