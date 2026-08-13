module Platform = struct
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
    val child : string -> string -> string
    val ensure_directory : string -> (unit, error) result Lwt.t
    val open_append : string -> (file, error) result Lwt.t

    val write :
      file -> string -> offset:int -> length:int -> (int, error) result Lwt.t

    val flush : file -> (unit, error) result Lwt.t
    val close : file -> (unit, error) result Lwt.t
    val pp_error : Format.formatter -> error -> unit
  end
end

module Make (Platform : Platform.S) = struct
  module IO = struct
    type 'a t = 'a Lwt.t
    type file = Platform.file
    type error = Platform.error
    type lock = Platform.lock
    type notifier = Platform.notifier

    let return = Lwt.return
    let bind = Lwt.bind
    let catch = Lwt.catch
    let async = Lwt.async
    let create_lock = Platform.create_lock
    let with_lock = Platform.with_lock
    let create_notifier = Platform.create_notifier
    let await notifier = Lwt.protected (Platform.await notifier)
    let notify = Platform.notify
    let dispose = Platform.dispose
    let child = Platform.child
    let ensure_directory = Platform.ensure_directory
    let open_append = Platform.open_append
    let write = Platform.write
    let flush = Platform.flush
    let close = Platform.close
    let pp_error = Platform.pp_error
  end

  include Observe_fs.Make (IO)
end
