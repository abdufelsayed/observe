module Direct = struct
  type 'a t = 'a
  type file = unit
  type error = unit
  type lock = unit
  type notifier = unit

  let return value = value
  let bind value callback = callback value
  let catch callback _ = callback ()
  let async callback = ignore (callback ())
  let create_lock () = ()
  let with_lock () callback = callback ()
  let create_notifier () = ()
  let await () = ()
  let notify () = ()
  let dispose () = ()
  let child = Filename.concat
  let ensure_directory _ = Ok ()
  let open_append _ = Ok ()
  let write () _ ~offset:_ ~length = Ok length
  let flush () = Ok ()
  let close () = Ok ()
  let pp_error _ () = ()
end

module Writer = Observe_fs.Make (Direct)

let _ = Writer.pp_error
