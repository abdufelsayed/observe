type writer = Unix.file_descr -> string -> int -> int -> int

val all : write:writer -> Unix.file_descr -> string -> unit
