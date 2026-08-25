type error =
  | Open_failed
  | Write_failed
  | Flush_failed
  | Close_failed
  | Closed_file

type file = { path : string; mutable closed : bool }
type lock = unit

type notifier = {
  id : int;
  mutable waiters : unit Lwt.u list;
  mutable disposed : bool;
}

let files : (string, Buffer.t) Hashtbl.t = Hashtbl.create 7
let max_write = ref max_int
let next_open_fails = ref false
let next_write_fails = ref false
let next_flush_fails = ref false
let next_close_fails = ref false
let next_child_callback : (unit -> unit) option ref = ref None
let write_gate : unit Lwt.t option ref = ref None
let flushes = ref 0
let closes = ref 0
let recorded_operations = ref []
let next_notifier_id = ref 0
let notification_counts : (int, int) Hashtbl.t = Hashtbl.create 2
let path_projections = ref 0

let reset () =
  Hashtbl.clear files;
  max_write := max_int;
  next_open_fails := false;
  next_write_fails := false;
  next_flush_fails := false;
  next_close_fails := false;
  next_child_callback := None;
  write_gate := None;
  flushes := 0;
  closes := 0;
  recorded_operations := [];
  next_notifier_id := 0;
  path_projections := 0;
  Hashtbl.clear notification_counts

let seed path value =
  let buffer = Buffer.create (String.length value + 32) in
  Buffer.add_string buffer value;
  Hashtbl.replace files path buffer

let contents path =
  match Hashtbl.find_opt files path with
  | None -> ""
  | Some buffer -> Buffer.contents buffer

let paths () = Hashtbl.to_seq_keys files |> List.of_seq |> List.sort compare
let set_max_write value = max_write := value
let fail_next_open () = next_open_fails := true
let fail_next_write () = next_write_fails := true
let fail_next_flush () = next_flush_fails := true
let fail_next_close () = next_close_fails := true
let path_projection_count () = !path_projections
let on_next_path callback = next_child_callback := Some callback
let flush_count () = !flushes
let close_count () = !closes
let operations () = List.rev !recorded_operations

let worker_notification_count () =
  Option.value ~default:0 (Hashtbl.find_opt notification_counts 0)

let block_writes () =
  let promise, wakener = Lwt.wait () in
  write_gate := Some promise;
  fun () ->
    write_gate := None;
    Lwt.wakeup_later wakener ()

module IO = struct
  type nonrec file = file

  type nonrec error = error =
    | Open_failed
    | Write_failed
    | Flush_failed
    | Close_failed
    | Closed_file

  type nonrec lock = lock
  type nonrec notifier = notifier

  let create_lock () = ()
  let with_lock () callback = callback ()

  let create_notifier () =
    let id = !next_notifier_id in
    incr next_notifier_id;
    { id; waiters = []; disposed = false }

  let await notifier =
    if notifier.disposed then Lwt.return_unit
    else
      let promise, wakener = Lwt.wait () in
      notifier.waiters <- wakener :: notifier.waiters;
      promise

  let wake waiter =
    try Lwt.wakeup_later waiter () with Invalid_argument _ -> ()

  let notify notifier =
    Hashtbl.replace notification_counts notifier.id
      (1
      + Option.value ~default:0
          (Hashtbl.find_opt notification_counts notifier.id));
    let waiters = notifier.waiters in
    notifier.waiters <- [];
    List.iter wake waiters

  let dispose notifier =
    notifier.disposed <- true;
    notify notifier

  let child ~dir ~name =
    incr path_projections;
    let callback = !next_child_callback in
    next_child_callback := None;
    Option.iter (fun callback -> callback ()) callback;
    Filename.concat dir name

  let ensure_directory _ = Lwt.return (Ok ())

  let open_append path =
    if !next_open_fails then (
      next_open_fails := false;
      Lwt.return (Error Open_failed))
    else (
      if not (Hashtbl.mem files path) then
        Hashtbl.add files path (Buffer.create 64);
      Lwt.return (Ok { path; closed = false }))

  let perform_write file bytes ~offset ~length =
    if file.closed then Lwt.return (Error Closed_file)
    else if !next_write_fails then (
      next_write_fails := false;
      Lwt.return (Error Write_failed))
    else
      let written = min length !max_write in
      recorded_operations := `Write :: !recorded_operations;
      if written >= 0 && written <= length then
        Buffer.add_substring (Hashtbl.find files file.path) bytes offset written;
      Lwt.return (Ok written)

  let write file bytes ~offset ~length =
    match !write_gate with
    | None -> perform_write file bytes ~offset ~length
    | Some gate ->
        Lwt.bind gate (fun () -> perform_write file bytes ~offset ~length)

  let flush file =
    if file.closed then Lwt.return (Error Closed_file)
    else if !next_flush_fails then (
      next_flush_fails := false;
      Lwt.return (Error Flush_failed))
    else (
      incr flushes;
      recorded_operations := `Flush :: !recorded_operations;
      Lwt.return (Ok ()))

  let close file =
    if file.closed then Lwt.return (Error Closed_file)
    else if !next_close_fails then (
      next_close_fails := false;
      Lwt.return (Error Close_failed))
    else (
      file.closed <- true;
      incr closes;
      recorded_operations := `Close :: !recorded_operations;
      Lwt.return (Ok ()))

  let pp_error formatter = function
    | Open_failed -> Format.pp_print_string formatter "open failed"
    | Write_failed -> Format.pp_print_string formatter "write failed"
    | Flush_failed -> Format.pp_print_string formatter "flush failed"
    | Close_failed -> Format.pp_print_string formatter "close failed"
    | Closed_file -> Format.pp_print_string formatter "closed file"
end
