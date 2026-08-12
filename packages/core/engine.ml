type message =
  | Text of { tag : string; message : string }
  | Lazy_text of { tag : string; message : unit -> string }
  | Free of (unit -> Value.t)
  | Structured : 'a Type.t * 'a -> message

let text ~tag message = Text { tag; message }
let text_lazy ~tag message = Lazy_text { tag; message }
let free make = Free make
let structured description value = Structured (description, value)

type clock_error = Unavailable
type console_acceptance = Accepted | Rejected
type 'a contained = Returned of 'a | Raised

let contain ~is_control_exception callback =
  try Returned (callback ())
  with exception_raised -> (
    let backtrace = Printexc.get_raw_backtrace () in
    match exception_raised with
    | Out_of_memory | Stack_overflow | Sys.Break ->
        Printexc.raise_with_backtrace exception_raised backtrace
    | _ when is_control_exception exception_raised ->
        Printexc.raise_with_backtrace exception_raised backtrace
    | _ -> Raised)

type production = {
  console : string -> console_acceptance;
  drains : Drain.t list;
  formatter : Formatter.t;
  silent : bool;
}

type output = Production of production | Capture_only of Capture.t

type t = {
  config : Config.t;
  clock : unit -> (Instant.t, clock_error) result;
  is_control_exception : exn -> bool;
  output : output;
}

let create_production config ~console_style ~clock ~console
    ~is_control_exception =
  let formatter =
    if Config.pretty config then Formatter.readable console_style
    else Formatter.json
  in
  {
    config;
    clock;
    is_control_exception;
    output =
      Production
        {
          console;
          drains = Config.drains config;
          formatter;
          silent = Config.silent config;
        };
  }

let create_capture config ~clock ~is_control_exception capture =
  { config; clock; is_control_exception; output = Capture_only capture }

let record t kind =
  match t.output with
  | Production _ -> Diagnostics.record kind
  | Capture_only capture -> Capture.record capture kind

let after_production_install t =
  match t.output with
  | Production { silent = true; drains = []; _ } when Config.enabled t.config ->
      Diagnostics.record Diagnostics.No_output
  | Production _ | Capture_only _ -> ()

let admitted config level =
  Config.enabled config && Level.compare level (Config.min_level config) >= 0

let author_payload t = function
  | Text { tag; message } -> Returned (Log.Text { tag; message })
  | Lazy_text { tag; message } -> (
      match
        contain ~is_control_exception:t.is_control_exception (fun () ->
            message ())
      with
      | Returned message -> Returned (Log.Text { tag; message })
      | Raised -> Raised)
  | Free make -> (
      match contain ~is_control_exception:t.is_control_exception make with
      | Returned value -> Returned (Log.Free value)
      | Raised -> Raised)
  | Structured (description, value) ->
      Returned (Log.Structured (description, value))

let seal t level instant payload =
  Log.Producer.make ~service:(Config.service t.config)
    ?environment:(Config.environment t.config)
    ?version:(Config.version t.config) ~instant ~level payload

let offer_capture t capture log = ignore (Capture.offer capture log)

let offer_console t console formatter log =
  match
    contain ~is_control_exception:t.is_control_exception (fun () ->
        Formatter.format formatter log)
  with
  | Raised -> record t Diagnostics.Formatting_raised
  | Returned (Error _) -> record t Diagnostics.Formatting_failed
  | Returned (Ok output) -> (
      match
        contain ~is_control_exception:t.is_control_exception (fun () ->
            console (output ^ "\n"))
      with
      | Returned Accepted -> ()
      | Returned Rejected -> record t Diagnostics.Console_rejected
      | Raised -> record t Diagnostics.Console_raised)

let offer_drain t drain log =
  match
    contain ~is_control_exception:t.is_control_exception (fun () ->
        Drain.offer drain log)
  with
  | Returned Drain.Accepted -> ()
  | Returned Drain.Rejected -> record t Diagnostics.Drain_rejected
  | Raised -> record t Diagnostics.Drain_raised

let deliver t log =
  match t.output with
  | Capture_only capture -> offer_capture t capture log
  | Production { console; drains; formatter; silent } ->
      if not silent then offer_console t console formatter log;
      List.iter (fun drain -> offer_drain t drain log) drains

let emit t level message =
  if admitted t.config level then
    match
      contain ~is_control_exception:t.is_control_exception (fun () ->
          t.clock ())
    with
    | Raised -> record t Diagnostics.Clock_raised
    | Returned (Error Unavailable) -> record t Diagnostics.Clock_unavailable
    | Returned (Ok instant) -> (
        match author_payload t message with
        | Raised -> record t Diagnostics.Authoring_raised
        | Returned payload -> deliver t (seal t level instant payload))
