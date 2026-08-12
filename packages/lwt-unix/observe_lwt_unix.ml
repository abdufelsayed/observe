module Clock = struct
  let nanoseconds_per_day = 86_400_000_000_000L
  let picoseconds_per_nanosecond = 1_000L

  let instant_of_day_and_picoseconds (days, picoseconds) =
    let days = Int64.of_int days in
    if
      Int64.compare days 0L < 0
      || Int64.compare picoseconds 0L < 0
      || Int64.compare days (Int64.div Int64.max_int nanoseconds_per_day) > 0
    then Error Observe.IO.Unavailable
    else
      let day_nanoseconds = Int64.mul days nanoseconds_per_day in
      let subday_nanoseconds =
        Int64.div picoseconds picoseconds_per_nanosecond
      in
      if
        Int64.compare subday_nanoseconds
          (Int64.sub Int64.max_int day_nanoseconds)
        > 0
      then Error Observe.IO.Unavailable
      else
        Ok
          (Observe.Instant.of_epoch_nanoseconds
             (Int64.add day_nanoseconds subday_nanoseconds))

  let now () = instant_of_day_and_picoseconds (Ptime_clock.now_d_ps ())
end

module Console = struct
  let lowercase = String.lowercase_ascii

  let contains ~needle haystack =
    let needle_length = String.length needle in
    let haystack_length = String.length haystack in
    let rec search offset =
      offset + needle_length <= haystack_length
      && (String.sub haystack offset needle_length = needle
         || search (offset + 1))
    in
    needle_length = 0 || search 0

  let capable_style ~term =
    match Option.map lowercase (Sys.getenv_opt "COLORTERM") with
    | Some ("truecolor" | "24bit") -> Observe.Formatter.Truecolor
    | Some _ | None ->
        if
          String.equal term "xterm-ghostty"
          || contains ~needle:"truecolor" term
          || contains ~needle:"-direct" term
        then Observe.Formatter.Truecolor
        else if contains ~needle:"256color" term then Observe.Formatter.Ansi_256
        else Observe.Formatter.Ansi_16

  let style () =
    try
      if not (Unix.isatty Unix.stderr) then Observe.Formatter.Plain
      else if Option.is_some (Sys.getenv_opt "NO_COLOR") then
        Observe.Formatter.Plain
      else
        match Option.map lowercase (Sys.getenv_opt "TERM") with
        | None | Some "" | Some "dumb" -> Observe.Formatter.Plain
        | Some term -> capable_style ~term
    with
    | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> raise exn
    | _ -> Observe.Formatter.Plain

  let rec write_all value offset remaining =
    if remaining > 0 then
      match Unix.write_substring Unix.stderr value offset remaining with
      | 0 -> raise (Sys_error "Observe console write made no progress")
      | written -> write_all value (offset + written) (remaining - written)
      | exception Unix.Unix_error (Unix.EINTR, _, _) ->
          write_all value offset remaining

  let write value =
    write_all value 0 (String.length value);
    Observe.IO.Accepted
end

module Observer = Observe.Make (Observe_lwt.IO)

let io =
  Observe_lwt.create ~clock:Clock.now ~console_style:Console.style
    ~write_console:Console.write ()

let observer = Observer.create io
let init config = Observer.init observer config
let init_exn config = Observer.init_exn observer config

module Test = struct
  exception Capture_error of Observe.capture_error

  let with_capture config ?capacity callback =
    Lwt.bind (Observer.with_capture observer config ?capacity callback)
      (function
      | Ok result -> Lwt.return result
      | Error error -> Lwt.fail (Capture_error error))
end
