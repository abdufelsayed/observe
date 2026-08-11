module Runtime = struct
  type +'a t = 'a Lwt.t
  type context = unit
  type 'a key = 'a Lwt.key

  let return = Lwt.return
  let bind = Lwt.bind
  let create_key = Lwt.new_key
  let get () key = Lwt.get key

  let with_binding () key value callback =
    Lwt.with_value key (Some value) callback

  let protect () ~finally callback =
    Lwt.finalize callback (fun () ->
        finally ();
        Lwt.return_unit)

  let is_control_exception () = function Lwt.Canceled -> true | _ -> false
end
