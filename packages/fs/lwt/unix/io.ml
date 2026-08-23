type operation = Inspect | Create_directory | Open | Write | Close
type error = { operation : operation; path : string; cause : Unix.error }
type file = { path : string; descriptor : Lwt_unix.file_descr }
type lock = Mutex.t

type notifier = {
  lock : Mutex.t;
  mutable waiters : unit Lwt.u list;
  mutable disposed : bool;
  mutable notification : Lwt_unix.notification option;
}

let with_lock lock callback =
  Mutex.lock lock;
  match callback () with
  | result ->
      Mutex.unlock lock;
      result
  | exception exn ->
      Mutex.unlock lock;
      raise exn

let create_lock () = Mutex.create ()
let wake waiter = try Lwt.wakeup_later waiter () with Invalid_argument _ -> ()

let dispatch notifier =
  let waiters, notification =
    with_lock notifier.lock (fun () ->
        let waiters = notifier.waiters in
        notifier.waiters <- [];
        let notification =
          if notifier.disposed then (
            let notification = notifier.notification in
            notifier.notification <- None;
            notification)
          else None
        in
        (waiters, notification))
  in
  List.iter wake waiters;
  Option.iter Lwt_unix.stop_notification notification

let create_notifier () =
  let notifier =
    {
      lock = Mutex.create ();
      waiters = [];
      disposed = false;
      notification = None;
    }
  in
  let notification = Lwt_unix.make_notification (fun () -> dispatch notifier) in
  notifier.notification <- Some notification;
  notifier

let await notifier =
  with_lock notifier.lock (fun () ->
      if notifier.disposed then Lwt.return_unit
      else
        let promise, wakener = Lwt.wait () in
        notifier.waiters <- wakener :: notifier.waiters;
        promise)

let send notifier =
  let notification =
    with_lock notifier.lock (fun () -> notifier.notification)
  in
  Option.iter
    (fun notification ->
      try Lwt_unix.send_notification notification
      with Invalid_argument _ -> ())
    notification

let notify = send

let dispose notifier =
  let should_send =
    with_lock notifier.lock (fun () ->
        if notifier.disposed then false
        else (
          notifier.disposed <- true;
          true))
  in
  if should_send then send notifier

let child ~dir ~name = Filename.concat dir name

let operation_name = function
  | Inspect -> "inspect"
  | Create_directory -> "create-directory"
  | Open -> "open"
  | Write -> "write"
  | Close -> "close"

let pp_error formatter { operation; path; cause } =
  Format.fprintf formatter "%s %S: %s" (operation_name operation) path
    (Unix.error_message cause)

let error operation path cause = Error { operation; path; cause }

let stat path =
  Lwt.catch
    (fun () -> Lwt.map Result.ok (Lwt_unix.stat path))
    (function
      | Unix.Unix_error (cause, _, _) -> Lwt.return (error Inspect path cause)
      | exn -> Lwt.fail exn)

let rec ensure_directory path =
  Lwt.bind (stat path) (function
    | Ok stats when stats.Unix.st_kind = Unix.S_DIR -> Lwt.return (Ok ())
    | Ok _ -> Lwt.return (error Inspect path Unix.ENOTDIR)
    | Error { cause = Unix.ENOENT; _ } ->
        let parent = Filename.dirname path in
        let prepare_parent =
          if String.equal parent path then Lwt.return (Ok ())
          else ensure_directory parent
        in
        Lwt.bind prepare_parent (function
          | Error _ as failure -> Lwt.return failure
          | Ok () ->
              Lwt.catch
                (fun () -> Lwt.map Result.ok (Lwt_unix.mkdir path 0o750))
                (function
                  | Unix.Unix_error (Unix.EEXIST, _, _) ->
                      Lwt.bind (stat path) (function
                        | Ok stats when stats.Unix.st_kind = Unix.S_DIR ->
                            Lwt.return (Ok ())
                        | Ok _ -> Lwt.return (error Inspect path Unix.ENOTDIR)
                        | Error _ as failure -> Lwt.return failure)
                  | Unix.Unix_error (cause, _, _) ->
                      Lwt.return (error Create_directory path cause)
                  | exn -> Lwt.fail exn))
    | Error _ as failure -> Lwt.return failure)

let open_append path =
  Lwt.catch
    (fun () ->
      Lwt.bind
        (Lwt_unix.openfile path
           [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ]
           0o640)
        (fun descriptor ->
          match Lwt_unix.set_blocking descriptor false with
          | () -> Lwt.return (Ok { path; descriptor })
          | exception exn ->
              Lwt.bind
                (Lwt.catch
                   (fun () -> Lwt_unix.close descriptor)
                   (fun _ -> Lwt.return_unit))
                (fun () -> Lwt.fail exn)))
    (function
      | Unix.Unix_error (cause, _, _) -> Lwt.return (error Open path cause)
      | exn -> Lwt.fail exn)

let rec write file bytes ~offset ~length =
  Lwt.catch
    (fun () ->
      Lwt.map Result.ok
        (Lwt_unix.write_string file.descriptor bytes offset length))
    (function
      | Unix.Unix_error (Unix.EINTR, _, _) -> write file bytes ~offset ~length
      | Unix.Unix_error (cause, _, _) ->
          Lwt.return (error Write file.path cause)
      | exn -> Lwt.fail exn)

let flush _file = Lwt.return (Ok ())

let close file =
  Lwt.catch
    (fun () -> Lwt.map Result.ok (Lwt_unix.close file.descriptor))
    (function
      | Unix.Unix_error (cause, _, _) ->
          Lwt.return (error Close file.path cause)
      | exn -> Lwt.fail exn)
