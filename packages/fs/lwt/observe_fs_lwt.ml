module IO = struct
  module type S = sig
    type file
    type error
    type lock
    type notifier

    val create_lock : unit -> lock
    val with_lock : lock -> (unit -> 'a) -> 'a
    val create_notifier : unit -> notifier
    val await : notifier -> unit Lwt.t
    val notify : notifier -> unit
    val dispose : notifier -> unit
    val child : dir:string -> name:string -> string
    val ensure_directory : string -> (unit, error) result Lwt.t
    val open_append : string -> (file, error) result Lwt.t

    val write :
      file -> string -> offset:int -> length:int -> (int, error) result Lwt.t

    val flush : file -> (unit, error) result Lwt.t
    val close : file -> (unit, error) result Lwt.t
    val pp_error : Format.formatter -> error -> unit
  end
end

module Make (IO : IO.S) = struct
  include Observe_fs.Make (struct
    type 'a t = 'a Lwt.t
    type file = IO.file
    type error = IO.error
    type lock = IO.lock
    type notifier = IO.notifier

    let return = Lwt.return
    let bind = Lwt.bind
    let catch = Lwt.catch
    let async = Lwt.async
    let create_lock = IO.create_lock
    let with_lock = IO.with_lock
    let create_notifier = IO.create_notifier
    let await notifier = Lwt.protected (IO.await notifier)
    let notify = IO.notify
    let dispose = IO.dispose
    let child ~dir ~name = IO.child ~dir ~name
    let ensure_directory = IO.ensure_directory
    let open_append = IO.open_append
    let write = IO.write
    let flush = IO.flush
    let close = IO.close
    let pp_error = IO.pp_error
  end)
end
