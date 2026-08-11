val style :
  isatty:(unit -> bool) ->
  getenv:(string -> string option) ->
  Observe.Formatter.style
(** Detect the maximum color capability from passive terminal signals. The
    detector performs no terminal I/O and returns [Plain] on probe failure. *)
