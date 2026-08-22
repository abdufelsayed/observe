type suite = Component | Core | Lwt_unix | Fs_lwt_unix
type t

val all : t list
val name : t -> string
val suite : t -> suite
val suite_name : suite -> string
val boundary : t -> string
val payload : t -> string
val logical_operations : t -> int
val find : string -> t option

val with_operation :
  t ->
  ((unit -> unit) -> (unit -> float option) -> (unit -> float option) -> 'a) ->
  'a
