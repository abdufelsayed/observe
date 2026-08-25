module IO = struct
  include Observe_fs_test_support.Fs_fixture.IO

  type 'a t = 'a Lwt.t

  let return = Lwt.return
  let bind = Lwt.bind
  let catch = Lwt.catch
  let async = Lwt.async
end

module Writer = Observe_fs.Make (IO)
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
      ~monotonic_now:(fun () -> Ok 0L)
      ~next_id:(fun () -> Ok "filesystem-operation")
      ~console_style:(fun () -> Observe.Formatter.Plain)
      ~offer_console:(fun _ -> Observe.IO.Rejected)
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
  match Lwt_main.run (Writer.create ~dir:"/logs" ?capacity ()) with
  | Ok writer -> writer
  | Error error -> fail "create failed: %a" Writer.pp_error error

let emit tag message = Observe.Logs.info (fun m -> m.text ~tag "%s" message)

let daily () =
  let writer = create () in
  install [ -1L; 0L; 86_400_000_000_000L; 0L ] (Writer.drain writer);
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
  Lwt_main.run (Writer.flush writer) |> Result.get_ok;
  let epoch =
    Observe_fs_test_support.Fs_fixture.contents "/logs/1970-01-01.jsonl"
  in
  check (line_count epoch = 2) "day switch-back lost a record: %S" epoch;
  check (contains epoch "\"tag\":\"epoch\"") "epoch record missing";
  check (contains epoch "\"tag\":\"back\"") "switch-back record missing";
  Lwt_main.run (Writer.shutdown writer) |> Result.get_ok

let append_and_partial () =
  Observe_fs_test_support.Fs_fixture.reset ();
  Observe_fs_test_support.Fs_fixture.seed "/logs/1970-01-01.jsonl"
    "{\"existing\":true}\n";
  Observe_fs_test_support.Fs_fixture.set_max_write 3;
  let writer = Lwt_main.run (Writer.create ~dir:"/logs" ()) |> Result.get_ok in
  install [ 0L ] (Writer.drain writer);
  emit "partial" "complete me";
  Lwt_main.run (Writer.shutdown writer) |> Result.get_ok;
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

type sample_builder = {
  typed : sample Observe.Schema.patch -> sample Observe.Schema.patch;
}

let sample_schema =
  Observe.Schema.record sample_t ~builder:(fun _ -> { typed = Fun.id })

let mutation () =
  let writer = create () in
  install [ 0L ] (Writer.drain writer);
  let sample = { value = "before" } in
  Observe.Logs.info (fun m -> m.typed ~using:sample_schema sample);
  sample.value <- "after";
  Lwt_main.run (Writer.shutdown writer) |> Result.get_ok;
  let output =
    Observe_fs_test_support.Fs_fixture.contents "/logs/1970-01-01.jsonl"
  in
  check
    (contains output "\"value\":\"before\"")
    "queue retained mutable consumer data: %S" output;
  check (not (contains output "after")) "mutation changed queued output"

let projection () =
  let writer = create () in
  let expected = Buffer.create 256 in
  let witness =
    Observe.Drain.create (fun log ->
        match Observe.Formatter.format Observe.Formatter.ndjson log with
        | Error _ -> Observe.Drain.Rejected
        | Ok bytes ->
            Buffer.add_string expected bytes;
            Observe.Drain.Accepted)
  in
  install ~extra_drains:[ witness ] [ 0L; 1L; 2L; 3L; 4L ] (Writer.drain writer);
  emit "text" "text payload";
  Observe.Logs.info (fun m ->
      let open Observe.Logs in
      m.untyped
      |+ m.field "action" Observe.Type.string "untyped"
      |+ m.field "count" Observe.Type.int 2
      |> m.seal);
  Observe.Logs.info (fun m ->
      m.typed ~using:sample_schema { value = "typed payload" });
  let wide = Observe.Logs.create ~name:"filesystem-wide" () in
  Observe.Logs.info ~operation:wide (fun m ->
      m.text ~tag:"correlated" "%s" "point payload");
  Observe.Logs.set wide (fun m ->
      let open Observe.Logs in
      m.untyped |+ m.field "result" Observe.Type.string "completed" |> m.seal);
  Observe.Logs.emit wide;
  Lwt_main.run (Writer.shutdown writer) |> Result.get_ok;
  let actual =
    Observe_fs_test_support.Fs_fixture.contents "/logs/1970-01-01.jsonl"
  in
  check
    (String.equal actual (Buffer.contents expected))
    "filesystem changed semantic NDJSON:\nexpected %S\nactual   %S"
    (Buffer.contents expected) actual;
  check (line_count actual = 5) "filesystem lost an observation: %S" actual;
  check
    (contains actual "\"operation_id\":\"filesystem-operation\"")
    "filesystem lost point correlation: %S" actual;
  check
    (contains actual
       "\"operation\":\"filesystem-wide\",\"operation_id\":\"filesystem-operation\",\"duration_ms\":0")
    "filesystem lost the flat wide operation fields: %S" actual

let capacity () =
  let writer = create ~capacity:1 () in
  install [ 0L; 1L; 2L ] (Writer.drain writer);
  let release = Observe_fs_test_support.Fs_fixture.block_writes () in
  let before = diagnostic_count Observe.Diagnostics.Drain_rejected in
  emit "one" "one";
  Lwt_main.run (Lwt.pause ());
  emit "two" "two";
  let projections =
    Observe_fs_test_support.Fs_fixture.path_projection_count ()
  in
  emit "three" "three";
  check
    (diagnostic_count Observe.Diagnostics.Drain_rejected = before + 1)
    "full queue did not reject exactly the newest record";
  check
    (Observe_fs_test_support.Fs_fixture.path_projection_count () = projections)
    "full queue projected the rejected record";
  release ();
  Lwt_main.run (Writer.shutdown writer) |> Result.get_ok;
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
  let writer = create () in
  install (List.init 1_026 Int64.of_int) (Writer.drain writer);
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
  Lwt_main.run (Writer.shutdown writer) |> Result.get_ok;
  let output =
    Observe_fs_test_support.Fs_fixture.contents "/logs/1970-01-01.jsonl"
  in
  check (line_count output = 1_025) "default capacity changed accepted work"

let flush_barriers () =
  let writer = create () in
  install [ 0L; 1L ] (Writer.drain writer);
  emit "first" "first";
  let first = Writer.flush writer in
  emit "second" "second";
  let second = Writer.flush writer in
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
  Lwt_main.run (Writer.shutdown writer) |> Result.get_ok

let coalesced_writes () =
  let records = 100 in
  let writer = create ~capacity:records () in
  install (List.init records Int64.of_int) (Writer.drain writer);
  let release = Observe_fs_test_support.Fs_fixture.block_writes () in
  emit "coalesced" "0";
  Lwt_main.run (Lwt.pause ());
  for index = 1 to records - 1 do
    emit "coalesced" (Int.to_string index)
  done;
  let notifications =
    Observe_fs_test_support.Fs_fixture.worker_notification_count ()
  in
  check (notifications <= 1) "burst woke the background writer %d times"
    notifications;
  release ();
  Lwt_main.run (Writer.shutdown writer) |> Result.get_ok;
  let operations = Observe_fs_test_support.Fs_fixture.operations () in
  let writes =
    List.fold_left
      (fun count -> function `Write -> count + 1 | `Flush | `Close -> count)
      0 operations
  in
  check (writes = 2)
    "active write plus queued burst used %d writes instead of two" writes;
  check
    (operations = [ `Write; `Write; `Flush; `Close ])
    "coalesced write changed flush or close ordering";
  let lines =
    Observe_fs_test_support.Fs_fixture.contents "/logs/1970-01-01.jsonl"
    |> String.split_on_char '\n'
    |> List.filter (fun line -> String.length line > 0)
  in
  check (List.length lines = records) "coalesced write lost records";
  List.iteri
    (fun index line ->
      check
        (contains line (Format.asprintf "\"message\":\"%d\"" index))
        "coalesced write changed FIFO order at record %d: %S" index line)
    lines

let failure () =
  let writer = create () in
  let independent = ref 0 in
  let independent_drain =
    Observe.Drain.create (fun _ ->
        incr independent;
        Observe.Drain.Accepted)
  in
  install ~extra_drains:[ independent_drain ] [ 0L; 1L ] (Writer.drain writer);
  let failed_before =
    diagnostic_count Observe.Diagnostics.Drain_delivery_failed
  in
  let rejected_before = diagnostic_count Observe.Diagnostics.Drain_rejected in
  Observe_fs_test_support.Fs_fixture.fail_next_write ();
  emit "failure" "fail";
  let outcome = Lwt_main.run (Writer.flush writer) in
  check (Result.is_error outcome) "write failure did not fail flush";
  check
    (diagnostic_count Observe.Diagnostics.Drain_delivery_failed
    = failed_before + 1)
    "asynchronous failure was not diagnosed exactly once";
  emit "later" "reject";
  check
    (diagnostic_count Observe.Diagnostics.Drain_rejected = rejected_before + 1)
    "failed writer accepted a later record";
  check (!independent = 2) "filesystem failure stopped an independent drain"

let expect_failure setup =
  let writer = create () in
  install [ 0L ] (Writer.drain writer);
  setup ();
  emit "failure" "fail";
  check
    (Result.is_error (Lwt_main.run (Writer.flush writer)))
    "filesystem failure did not settle flush with an error"

let failure_open () =
  expect_failure Observe_fs_test_support.Fs_fixture.fail_next_open

let failure_zero_progress () =
  expect_failure (fun () -> Observe_fs_test_support.Fs_fixture.set_max_write 0)

let failure_flush () =
  expect_failure Observe_fs_test_support.Fs_fixture.fail_next_flush

let failure_close () =
  let writer = create () in
  install [ 0L ] (Writer.drain writer);
  emit "failure" "fail";
  Observe_fs_test_support.Fs_fixture.fail_next_close ();
  check
    (Result.is_error (Lwt_main.run (Writer.shutdown writer)))
    "close failure did not settle shutdown with an error"

let invalid_write_count () =
  let writer = create () in
  install [ 0L ] (Writer.drain writer);
  Observe_fs_test_support.Fs_fixture.set_max_write (-1);
  emit "invalid" "invalid write count";
  match Lwt_main.run (Writer.flush writer) with
  | Error (Writer.Invalid_write_count -1) -> ()
  | Error error -> fail "unexpected failure: %a" Writer.pp_error error
  | Ok () -> fail "invalid write count was accepted"

let projection_failure_releases_capacity () =
  let writer = create ~capacity:1 () in
  install [ 0L; 1L ] (Writer.drain writer);
  let raised_before = diagnostic_count Observe.Diagnostics.Drain_raised in
  Observe_fs_test_support.Fs_fixture.on_next_path (fun () ->
      failwith "path projection failed");
  emit "failed-projection" "reject";
  check
    (diagnostic_count Observe.Diagnostics.Drain_raised = raised_before + 1)
    "path projection failure did not escape through the drain boundary";
  emit "after-projection" "accepted";
  Lwt_main.run (Writer.shutdown writer) |> Result.get_ok;
  let output =
    Observe_fs_test_support.Fs_fixture.contents "/logs/1970-01-01.jsonl"
  in
  check (line_count output = 1) "projection failure leaked its reservation";
  check
    (contains output "\"tag\":\"after-projection\"")
    "later output was not accepted after projection failure"

let shutdown_during_projection () =
  let writer = create ~capacity:1 () in
  install [ 0L ] (Writer.drain writer);
  let shutdown = ref None in
  let rejected_before = diagnostic_count Observe.Diagnostics.Drain_rejected in
  Observe_fs_test_support.Fs_fixture.on_next_path (fun () ->
      shutdown := Some (Writer.shutdown writer));
  emit "shutdown-race" "reject";
  check
    (diagnostic_count Observe.Diagnostics.Drain_rejected = rejected_before + 1)
    "shutdown race accepted a reserved projection";
  let shutdown =
    match !shutdown with
    | Some shutdown -> shutdown
    | None -> fail "path projection did not start shutdown"
  in
  Lwt_main.run shutdown |> Result.get_ok;
  check
    (Observe_fs_test_support.Fs_fixture.paths () = [])
    "shutdown race enqueued a projected record";
  check
    (Observe_fs_test_support.Fs_fixture.close_count () = 0)
    "empty shutdown unexpectedly opened a file"

let shutdown () =
  let writer = create () in
  install [ 0L; 1L ] (Writer.drain writer);
  emit "before" "before";
  Lwt_main.run (Writer.shutdown writer) |> Result.get_ok;
  Lwt_main.run (Writer.shutdown writer) |> Result.get_ok;
  let rejected_before = diagnostic_count Observe.Diagnostics.Drain_rejected in
  let projections =
    Observe_fs_test_support.Fs_fixture.path_projection_count ()
  in
  emit "after" "after";
  check
    (diagnostic_count Observe.Diagnostics.Drain_rejected = rejected_before + 1)
    "shutdown writer accepted later output";
  check
    (Observe_fs_test_support.Fs_fixture.path_projection_count () = projections)
    "shutdown writer projected later output";
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
  | [ _; "coalesced-writes" ] -> coalesced_writes ()
  | [ _; "failure" ] -> failure ()
  | [ _; "failure-open" ] -> failure_open ()
  | [ _; "failure-zero-progress" ] -> failure_zero_progress ()
  | [ _; "failure-flush" ] -> failure_flush ()
  | [ _; "failure-close" ] -> failure_close ()
  | [ _; "invalid-write-count" ] -> invalid_write_count ()
  | [ _; "projection-failure-releases-capacity" ] ->
      projection_failure_releases_capacity ()
  | [ _; "shutdown-during-projection" ] -> shutdown_during_projection ()
  | [ _; "shutdown" ] -> shutdown ()
  | _ -> fail "unknown portable filesystem scenario"
