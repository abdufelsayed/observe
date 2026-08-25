type offer = Accepted | Full | Closed
type status = Running | Closing | Stopped of exn option

type t = {
  descriptor : Lwt_unix.file_descr;
  capacity : int;
  mutex : Mutex.t;
  queue : (int64 * string) Queue.t;
  mutable accepted_sequence : int64;
  mutable completed_sequence : int64;
  mutable status : status;
  mutable active : bool;
  mutable worker_waiter : unit Lwt.u option;
  mutable notification_pending : bool;
  mutable flush_waiters : (int64 * unit Lwt.u) list;
  shutdown_promise : unit Lwt.t;
  shutdown_wakener : unit Lwt.u;
  mutable notification : Lwt_unix.notification option;
}

let with_lock t callback =
  Mutex.lock t.mutex;
  match callback () with
  | result ->
      Mutex.unlock t.mutex;
      result
  | exception exn ->
      Mutex.unlock t.mutex;
      raise exn

let wake waiters = List.iter (fun waiter -> Lwt.wakeup_later waiter ()) waiters

let fail waiters exn =
  List.iter (fun (_, waiter) -> Lwt.wakeup_later_exn waiter exn) waiters

let stop_notification t =
  let notification =
    with_lock t (fun () ->
        let notification = t.notification in
        t.notification <- None;
        notification)
  in
  Option.iter Lwt_unix.stop_notification notification

let notify t =
  with_lock t (fun () ->
      match (t.worker_waiter, t.notification, t.notification_pending) with
      | Some _, Some notification, false ->
          t.notification_pending <- true;
          Lwt_unix.send_notification notification
      | None, _, _ | _, None, _ | _, _, true -> ())

let complete_waiters t =
  let ready =
    with_lock t (fun () ->
        let ready, pending =
          List.partition
            (fun (target, _) -> Int64.compare target t.completed_sequence <= 0)
            t.flush_waiters
        in
        t.flush_waiters <- pending;
        List.map snd ready)
  in
  wake ready

let rec write_all descriptor value offset =
  let remaining = String.length value - offset in
  if remaining = 0 then Lwt.return_unit
  else
    Lwt.catch
      (fun () ->
        Lwt.bind (Lwt_unix.write_string descriptor value offset remaining)
          (function
          | 0 -> Lwt.fail (Sys_error "Observe console write made no progress")
          | written -> write_all descriptor value (offset + written)))
      (function
        | Unix.Unix_error (Unix.EINTR, _, _) ->
            write_all descriptor value offset
        | exn -> Lwt.fail exn)

let close_with_error t exn =
  let flush_waiters, shutdown =
    with_lock t (fun () ->
        t.status <- Stopped (Some exn);
        t.active <- false;
        Queue.clear t.queue;
        let waiters = t.flush_waiters in
        t.flush_waiters <- [];
        (waiters, t.shutdown_wakener))
  in
  stop_notification t;
  fail flush_waiters exn;
  Lwt.wakeup_later_exn shutdown exn

let close_cleanly t =
  let flush_waiters, shutdown =
    with_lock t (fun () ->
        t.status <- Stopped None;
        let waiters = t.flush_waiters in
        t.flush_waiters <- [];
        (waiters, t.shutdown_wakener))
  in
  stop_notification t;
  wake (List.map snd flush_waiters);
  Lwt.wakeup_later shutdown ()

let rec worker t =
  let next =
    with_lock t (fun () ->
        match Queue.take_opt t.queue with
        | Some record ->
            t.active <- true;
            `Record record
        | None -> (
            t.active <- false;
            match t.status with
            | Closing -> `Close
            | Stopped _ -> `Stop
            | Running ->
                let promise, wakener = Lwt.wait () in
                t.worker_waiter <- Some wakener;
                `Wait promise))
  in
  match next with
  | `Record (sequence, record) ->
      Lwt.bind
        (Lwt.catch
           (fun () -> Lwt.map Result.ok (write_all t.descriptor record 0))
           (fun exn -> Lwt.return (Error exn)))
        (function
          | Ok () ->
              with_lock t (fun () ->
                  t.active <- false;
                  t.completed_sequence <- sequence);
              complete_waiters t;
              worker t
          | Error exn ->
              close_with_error t exn;
              Lwt.return_unit)
  | `Wait promise -> Lwt.bind promise (fun () -> worker t)
  | `Close ->
      complete_waiters t;
      close_cleanly t;
      Lwt.return_unit
  | `Stop -> Lwt.return_unit

let create ~capacity descriptor =
  if capacity <= 0 then
    invalid_arg "Observe_lwt_unix: capacity must be positive";
  let shutdown_promise, shutdown_wakener = Lwt.wait () in
  let t =
    {
      descriptor;
      capacity;
      mutex = Mutex.create ();
      queue = Queue.create ();
      accepted_sequence = 0L;
      completed_sequence = 0L;
      status = Running;
      active = false;
      worker_waiter = None;
      notification_pending = false;
      flush_waiters = [];
      shutdown_promise;
      shutdown_wakener;
      notification = None;
    }
  in
  let notification =
    Lwt_unix.make_notification (fun () ->
        let waiter =
          with_lock t (fun () ->
              t.notification_pending <- false;
              let waiter = t.worker_waiter in
              t.worker_waiter <- None;
              waiter)
        in
        Option.iter (fun waiter -> Lwt.wakeup_later waiter ()) waiter)
  in
  t.notification <- Some notification;
  Lwt.async (fun () -> worker t);
  t

let offer t record =
  let result =
    with_lock t (fun () ->
        match t.status with
        | Closing | Stopped _ -> Closed
        | Running when Queue.length t.queue >= t.capacity -> Full
        | Running ->
            let sequence = Int64.succ t.accepted_sequence in
            Queue.add (sequence, record) t.queue;
            t.accepted_sequence <- sequence;
            Accepted)
  in
  (match result with Accepted -> notify t | Full | Closed -> ());
  result

let flush t =
  with_lock t (fun () ->
      match t.status with
      | Stopped None -> Lwt.return_unit
      | Stopped (Some exn) -> Lwt.fail exn
      | (Running | Closing)
        when Int64.compare t.completed_sequence t.accepted_sequence >= 0 ->
          Lwt.return_unit
      | Running | Closing ->
          let promise, wakener = Lwt.wait () in
          t.flush_waiters <- (t.accepted_sequence, wakener) :: t.flush_waiters;
          Lwt.protected promise)

let shutdown t =
  let should_notify =
    with_lock t (fun () ->
        match t.status with
        | Running ->
            t.status <- Closing;
            true
        | Closing | Stopped _ -> false)
  in
  if should_notify then notify t;
  Lwt.protected t.shutdown_promise

let abort t =
  let waiter, shutdown =
    with_lock t (fun () ->
        match t.status with
        | Running ->
            t.status <- Stopped None;
            let waiter = t.worker_waiter in
            t.worker_waiter <- None;
            (waiter, Some t.shutdown_wakener)
        | Closing | Stopped _ -> (None, None))
  in
  stop_notification t;
  Option.iter (fun waiter -> Lwt.wakeup_later waiter ()) waiter;
  Option.iter (fun shutdown -> Lwt.wakeup_later shutdown ()) shutdown
