module Observer = Observe.Make (Observe_lwt.IO)

let text ~tag message (builder : Observe.Logs.builder) =
  builder.text ~tag "%s" message

let fail format = Format.kasprintf failwith format

let check condition format =
  Format.kasprintf
    (fun message -> if not condition then failwith message)
    format

let valid_json value =
  let decoder = Jsonm.decoder (`String value) in
  let rec decode saw_lexeme =
    match Jsonm.decode decoder with
    | `Lexeme _ -> decode true
    | `End -> saw_lexeme
    | `Await | `Error _ -> false
  in
  decode false

let json_string_field field value =
  let decoder = Jsonm.decoder (`String value) in
  let rec decode () =
    match Jsonm.decode decoder with
    | `Lexeme (`Name name) when String.equal name field -> decode_value ()
    | `Lexeme _ -> decode ()
    | `End | `Await | `Error _ -> None
  and decode_value () =
    match Jsonm.decode decoder with
    | `Lexeme (`String value) -> Some value
    | `Lexeme _ | `End | `Await | `Error _ -> None
  in
  decode ()

let nonempty_lines value =
  String.split_on_char '\n' value
  |> List.filter (fun line -> String.length line > 0)

let read_file path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in input)
    (fun () -> really_input_string input (in_channel_length input))

let with_directory callback =
  let root = Filename.temp_file "observe-fs" ".test" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Fun.protect
    ~finally:(fun () ->
      let rec remove path =
        if Sys.is_directory path then (
          Sys.readdir path
          |> Array.iter (fun child -> remove (Filename.concat path child));
          Unix.rmdir path)
        else Sys.remove path
      in
      if Sys.file_exists root then remove root)
    (fun () -> callback root)

let install timestamps drain =
  let timestamps = ref timestamps in
  let clock () =
    match !timestamps with
    | timestamp :: rest ->
        timestamps := rest;
        Ok (Observe.Timestamp.of_unix_ns timestamp)
    | [] -> fail "ready test clock exhausted"
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
  Observer.init_exn observer
    (Observe.Config.create_exn ~service:"ready-fs"
       ~console:Observe.Config.Silent ~drains:[ drain ] ())

let daily () =
  with_directory (fun root ->
      let logs = Filename.concat root "logs" in
      Unix.mkdir logs 0o700;
      let existing = Filename.concat logs "1970-01-01.jsonl" in
      let output = open_out_bin existing in
      output_string output "{\"existing\":true}\n";
      close_out output;
      let drain = Lwt_main.run (Observe_fs_lwt_unix.create_exn ~dir:logs ()) in
      install [ 0L; 86_400_000_000_000L; 1L ] drain;
      Observe.Logs.info (text ~tag:"epoch" "first");
      Lwt_main.run (Observe_lwt_unix.flush ());
      check
        (read_file existing
        |> nonempty_lines
        |> List.exists (fun line -> json_string_field "tag" line = Some "epoch")
        )
        "ready flush did not include filesystem output";
      Observe.Logs.warn (text ~tag:"tomorrow" "second");
      Observe.Logs.error (text ~tag:"back" "third");
      Lwt_main.run (Observe_lwt_unix.shutdown ());
      let first = read_file existing in
      let second = read_file (Filename.concat logs "1970-01-02.jsonl") in
      let lines = nonempty_lines first @ nonempty_lines second in
      check
        (String.starts_with ~prefix:"{\"existing\":true}\n" first)
        "ready writer truncated existing content";
      check
        (String.fold_left
           (fun count char -> if char = '\n' then count + 1 else count)
           0 first
        = 3)
        "ready writer lost switch-back output: %S" first;
      check (String.contains second '\n') "second daily file is empty";
      check
        (List.for_all valid_json lines)
        "ready output contains invalid NDJSON")

let recursive_directory () =
  with_directory (fun root ->
      let logs = Filename.concat root "a/b/c" in
      let drain = Lwt_main.run (Observe_fs_lwt_unix.create_exn ~dir:logs ()) in
      install [ 0L ] drain;
      Observe.Logs.info (text ~tag:"recursive" "created");
      Lwt_main.run (Observe_lwt_unix.shutdown ());
      check (Sys.is_directory logs) "nested log directory was not created";
      check
        (Sys.file_exists (Filename.concat logs "1970-01-01.jsonl"))
        "nested daily file was not written")

let invalid_target () =
  with_directory (fun root ->
      let target = Filename.concat root "file" in
      let output = open_out_bin target in
      close_out output;
      match Lwt_main.run (Observe_fs_lwt_unix.create ~dir:target ()) with
      | Error (Observe_fs_lwt_unix.Filesystem { cause = Unix.ENOTDIR; _ }) -> ()
      | Error error ->
          fail "unexpected setup error: %a" Observe_fs_lwt_unix.pp_error error
      | Ok _ -> fail "existing non-directory was accepted")

let lifecycle_closed () =
  Lwt_main.run (Observe_lwt_unix.shutdown ());
  with_directory (fun root ->
      (match Lwt_main.run (Observe_fs_lwt_unix.create ~dir:root ()) with
      | Error Observe_fs_lwt_unix.Lifecycle_closed -> ()
      | Error error ->
          fail "unexpected lifecycle error: %a" Observe_fs_lwt_unix.pp_error
            error
      | Ok _ -> fail "closed lifecycle accepted a new worker");
      check
        (Array.length (Sys.readdir root) = 0)
        "rejected lifecycle registration leaked filesystem output")

let concurrency () =
  with_directory (fun root ->
      let drain = Lwt_main.run (Observe_fs_lwt_unix.create_exn ~dir:root ()) in
      Observe_lwt_unix.init_exn
        (Observe.Config.create_exn ~service:"fs-concurrency"
           ~console:Observe.Config.Silent ~drains:[ drain ] ());
      let producers = 8 in
      let records_per_producer = 100 in
      let threads =
        List.init producers (fun producer ->
            Thread.create
              (fun () ->
                for record = 1 to records_per_producer do
                  Observe.Logs.info
                    (text ~tag:"concurrency"
                       (Format.sprintf "%d:%d" producer record))
                done)
              ())
      in
      List.iter Thread.join threads;
      Lwt_main.run (Observe_lwt_unix.shutdown ());
      let files = Sys.readdir root in
      check (Array.length files = 1) "concurrent output selected extra files";
      let output = read_file (Filename.concat root files.(0)) in
      let lines = nonempty_lines output in
      check
        (List.length lines = producers * records_per_producer)
        "concurrent output lost or duplicated records: expected %d, got %d"
        (producers * records_per_producer)
        (List.length lines);
      let messages = Hashtbl.create (List.length lines) in
      List.iter
        (fun line ->
          check (valid_json line)
            "concurrent output interleaved or corrupted a record: %S" line;
          match json_string_field "message" line with
          | None -> fail "concurrent record has no string message: %S" line
          | Some message ->
              check
                (not (Hashtbl.mem messages message))
                "concurrent output duplicated %S" message;
              Hashtbl.add messages message ())
        lines;
      for producer = 0 to producers - 1 do
        for record = 1 to records_per_producer do
          let message = Format.asprintf "%d:%d" producer record in
          check
            (Hashtbl.mem messages message)
            "concurrent output lost %S" message
        done
      done)

let () =
  match Array.to_list Sys.argv with
  | [ _; "daily" ] -> daily ()
  | [ _; "recursive-directory" ] -> recursive_directory ()
  | [ _; "invalid-target" ] -> invalid_target ()
  | [ _; "lifecycle-closed" ] -> lifecycle_closed ()
  | [ _; "concurrency" ] -> concurrency ()
  | _ -> fail "unknown ready filesystem scenario"
