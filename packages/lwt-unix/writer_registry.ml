type error = Closed
type identity = { shutdown_claimed : bool Atomic.t }

type hook = {
  identity : identity;
  flush : unit -> unit Lwt.t;
  shutdown : unit -> unit Lwt.t;
}

type status = Open | Closing of unit Lwt.t | Settled of (unit, exn) result
type t = { mutex : Mutex.t; mutable hooks : hook list; mutable status : status }

let with_lock t callback =
  Mutex.lock t.mutex;
  match callback () with
  | result ->
      Mutex.unlock t.mutex;
      result
  | exception exn ->
      Mutex.unlock t.mutex;
      raise exn

let create () = { mutex = Mutex.create (); hooks = []; status = Open }
let create_identity () = { shutdown_claimed = Atomic.make false }

let register t ~identity ~flush ~shutdown =
  with_lock t (fun () ->
      match t.status with
      | Open ->
          t.hooks <- { identity; flush; shutdown } :: t.hooks;
          Ok ()
      | Closing _ | Settled _ -> Error Closed)

let protected_call callback =
  match callback () with
  | promise ->
      Lwt.catch
        (fun () -> Lwt.map Result.ok (Lwt.protected promise))
        (fun exn -> Lwt.return (Error exn))
  | exception exn -> Lwt.return (Error exn)

let settle callbacks =
  let promises = List.map protected_call callbacks in
  Lwt.bind (Lwt.all promises) (fun outcomes ->
      match List.find_opt Result.is_error outcomes with
      | None -> Lwt.return_unit
      | Some (Error exn) -> Lwt.fail exn
      | Some (Ok ()) -> assert false)

let settle_shutdown hooks =
  hooks
  |> List.filter_map (fun hook ->
      if Atomic.compare_and_set hook.identity.shutdown_claimed false true then
        Some hook.shutdown
      else None)
  |> settle

let flush t =
  let action =
    with_lock t (fun () ->
        match t.status with
        | Open -> `Flush (List.rev t.hooks)
        | Closing promise -> `Wait promise
        | Settled (Ok ()) -> `Done
        | Settled (Error exn) -> `Failed exn)
  in
  match action with
  | `Flush hooks -> settle (List.map (fun hook -> hook.flush) hooks)
  | `Wait promise -> Lwt.protected promise
  | `Done -> Lwt.return_unit
  | `Failed exn -> Lwt.fail exn

let shutdown t =
  let action =
    with_lock t (fun () ->
        match t.status with
        | Open ->
            let promise, wakener = Lwt.wait () in
            t.status <- Closing promise;
            `Start (List.rev t.hooks, promise, wakener)
        | Closing promise -> `Wait promise
        | Settled (Ok ()) -> `Done
        | Settled (Error exn) -> `Failed exn)
  in
  match action with
  | `Start (hooks, promise, wakener) ->
      let work = settle_shutdown hooks in
      Lwt.on_any work
        (fun () ->
          with_lock t (fun () -> t.status <- Settled (Ok ()));
          Lwt.wakeup_later wakener ())
        (fun exn ->
          with_lock t (fun () -> t.status <- Settled (Error exn));
          Lwt.wakeup_later_exn wakener exn);
      Lwt.protected promise
  | `Wait promise -> Lwt.protected promise
  | `Done -> Lwt.return_unit
  | `Failed exn -> Lwt.fail exn
