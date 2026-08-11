type writer = Unix.file_descr -> string -> int -> int -> int

let rec loop ~write descriptor value offset remaining =
  if remaining > 0 then
    match write descriptor value offset remaining with
    | 0 -> raise (Sys_error "Observe terminal write made no progress")
    | written ->
        loop ~write descriptor value (offset + written) (remaining - written)
    | exception Unix.Unix_error (Unix.EINTR, _, _) ->
        loop ~write descriptor value offset remaining

let all ~write descriptor value =
  loop ~write descriptor value 0 (String.length value)
