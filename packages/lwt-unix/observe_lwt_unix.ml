module Clock = struct
  let nanoseconds_per_day = 86_400_000_000_000L
  let picoseconds_per_nanosecond = 1_000L

  let timestamp_of_day_and_picoseconds (days, picoseconds) =
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
          (Observe.Timestamp.of_unix_ns
             (Int64.add day_nanoseconds subday_nanoseconds))

  let now () = timestamp_of_day_and_picoseconds (Ptime_clock.now_d_ps ())
  let monotonic_now () = Ok (Mtime_clock.elapsed_ns ())
end

module Identity = struct
  let next_value = Atomic.make 0

  let next () =
    let value = Atomic.fetch_and_add next_value 1 + 1 in
    Ok ("operation-" ^ string_of_int value)
end

module Console = struct
  let lowercase = String.lowercase_ascii

  let contains ~needle haystack =
    let needle_length = String.length needle in
    let haystack_length = String.length haystack in
    let rec search offset =
      offset + needle_length <= haystack_length
      &&
      let rec matches index =
        index = needle_length
        || String.unsafe_get haystack (offset + index)
           = String.unsafe_get needle index
           && matches (index + 1)
      in
      matches 0 || search (offset + 1)
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

  let writer = Writer.create ~capacity:1_024 Lwt_unix.stderr

  let offer value =
    match Writer.offer writer value with
    | Accepted -> Observe.IO.Accepted
    | Full | Closed -> Observe.IO.Rejected

  let flush () = Writer.flush writer
  let shutdown () = Writer.shutdown writer
end

module Observer = Observe.Make (Observe_lwt.IO)

let writers =
  Writer_registry.create ~flush:Console.flush ~shutdown:Console.shutdown

let owner_thread = Thread.id (Thread.self ())

let io =
  Observe_lwt.create ~clock:Clock.now ~monotonic_now:Clock.monotonic_now
    ~next_id:Identity.next ~console_style:Console.style
    ~offer_console:Console.offer
    ~can_lookup_context:(fun () -> Thread.id (Thread.self ()) = owner_thread)
    ()

let observer = Observer.create io
let init config = Observer.init observer config
let init_exn config = Observer.init_exn observer config
let flush () = Writer_registry.flush writers
let shutdown () = Writer_registry.shutdown writers

module Lifecycle = struct
  type error = Writer_registry.error = Closed

  let register ~flush ~shutdown =
    Writer_registry.register writers ~flush ~shutdown
end

module Test = struct
  exception Capture_error of Observe.capture_error

  let with_capture_exn config ?capacity callback =
    Lwt.bind (Observer.with_capture observer config ?capacity callback)
      (function
      | Ok result -> Lwt.return result
      | Error error -> Lwt.fail (Capture_error error))
end
