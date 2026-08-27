module IO = struct
  type file = unit
  type error = unit
  type lock = unit
  type notifier = unit

  let create_lock () = ()
  let with_lock () callback = callback ()
  let create_notifier () = ()
  let await () = Lwt.return_unit
  let notify () = ()
  let dispose () = ()
  let child ~dir ~name = Filename.concat dir name
  let ensure_directory _ = Lwt.return (Ok ())
  let open_append _ = Lwt.return (Ok ())
  let write () _ ~offset:_ ~length = Lwt.return (Ok length)
  let flush () = Lwt.return (Ok ())
  let close () = Lwt.return (Ok ())
  let pp_error _ () = ()
end

module Writer = Observe_fs_lwt.Make (IO)

let delivery_facts writer = Writer.delivery_facts writer

let _ =
  (Writer.create, Writer.drain, delivery_facts, Writer.flush, Writer.shutdown)
