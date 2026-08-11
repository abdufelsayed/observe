let lowercase value = String.lowercase_ascii value

let contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec search offset =
    offset + needle_length <= haystack_length
    && (String.sub haystack offset needle_length = needle || search (offset + 1))
  in
  needle_length = 0 || search 0

let capable_style ~getenv term =
  match Option.map lowercase (getenv "COLORTERM") with
  | Some ("truecolor" | "24bit") -> Observe.Formatter.Truecolor
  | Some _ | None ->
      let term = Option.value ~default:"" term |> lowercase in
      if
        term = "xterm-ghostty"
        || contains ~needle:"truecolor" term
        || contains ~needle:"-direct" term
      then Observe.Formatter.Truecolor
      else if contains ~needle:"256color" term then Observe.Formatter.Ansi_256
      else Observe.Formatter.Ansi_16

let style ~isatty ~getenv =
  try
    if not (isatty ()) then Observe.Formatter.Plain
    else if Option.is_some (getenv "NO_COLOR") then Observe.Formatter.Plain
    else
      let term = getenv "TERM" in
      match term with
      | Some value when String.lowercase_ascii value = "dumb" ->
          Observe.Formatter.Plain
      | Some _ | None -> capable_style ~getenv term
  with
  | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> raise exn
  | _ -> Observe.Formatter.Plain
