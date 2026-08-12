type message =
  | Text of { tag : string; message : string }
  | Lazy_text of { tag : string; message : unit -> string }
  | Free of (unit -> Observe_value.t)
  | Structured : 'a Observe_type.t * 'a -> message

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
  drains : Observe_drain.t list;
  formatter : Observe_formatter.t;
  silent : bool;
}

type output = Production of production | Capture_only of Observe_capture.t

type t = {
  config : Observe_config.t;
  clock : unit -> (Observe_instant.t, clock_error) result;
  is_control_exception : exn -> bool;
  output : output;
}

let create_production config ~console_style ~clock ~console
    ~is_control_exception =
  let formatter =
    if Observe_config.pretty config then
      Observe_formatter.readable console_style
    else Observe_formatter.json
  in
  {
    config;
    clock;
    is_control_exception;
    output =
      Production
        {
          console;
          drains = Observe_config.drains config;
          formatter;
          silent = Observe_config.silent config;
        };
  }

let create_capture config ~clock ~is_control_exception capture =
  { config; clock; is_control_exception; output = Capture_only capture }

let record t kind =
  match t.output with
  | Production _ -> Observe_diagnostics.record kind
  | Capture_only capture -> Observe_capture.record capture kind

let after_production_install t =
  match t.output with
  | Production { silent = true; drains = []; _ }
    when Observe_config.enabled t.config ->
      Observe_diagnostics.record Observe_diagnostics.No_output
  | Production _ | Capture_only _ -> ()

let admitted config level =
  Observe_config.enabled config
  && Observe_level.compare level (Observe_config.min_level config) >= 0

let author_payload t = function
  | Text { tag; message } -> Returned (Observe_log.Text { tag; message })
  | Lazy_text { tag; message } -> (
      match
        contain ~is_control_exception:t.is_control_exception (fun () ->
            message ())
      with
      | Returned message -> Returned (Observe_log.Text { tag; message })
      | Raised -> Raised)
  | Free make -> (
      match contain ~is_control_exception:t.is_control_exception make with
      | Returned value -> Returned (Observe_log.Free value)
      | Raised -> Raised)
  | Structured (description, value) ->
      Returned (Observe_log.Structured (description, value))

let seal t level instant payload =
  Observe_log.Producer.make
    ~service:(Observe_config.service t.config)
    ?environment:(Observe_config.environment t.config)
    ?version:(Observe_config.version t.config)
    ~instant ~level payload

let offer_capture t capture log = ignore (Observe_capture.offer capture log)

let offer_console t console formatter log =
  match
    contain ~is_control_exception:t.is_control_exception (fun () ->
        Observe_formatter.format formatter log)
  with
  | Raised -> record t Observe_diagnostics.Formatting_raised
  | Returned (Error _) -> record t Observe_diagnostics.Formatting_failed
  | Returned (Ok output) -> (
      match
        contain ~is_control_exception:t.is_control_exception (fun () ->
            console (output ^ "\n"))
      with
      | Returned Accepted -> ()
      | Returned Rejected -> record t Observe_diagnostics.Console_rejected
      | Raised -> record t Observe_diagnostics.Console_raised)

let offer_drain t drain log =
  match
    contain ~is_control_exception:t.is_control_exception (fun () ->
        Observe_drain.offer drain log)
  with
  | Returned Observe_drain.Accepted -> ()
  | Returned Observe_drain.Rejected ->
      record t Observe_diagnostics.Drain_rejected
  | Raised -> record t Observe_diagnostics.Drain_raised

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
    | Raised -> record t Observe_diagnostics.Clock_raised
    | Returned (Error Unavailable) ->
        record t Observe_diagnostics.Clock_unavailable
    | Returned (Ok instant) -> (
        match author_payload t message with
        | Raised -> record t Observe_diagnostics.Authoring_raised
        | Returned payload -> deliver t (seal t level instant payload))
