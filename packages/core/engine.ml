type 'a contained = Returned of 'a | Raised

let contain ~is_control_exception callback =
  match callback () with
  | result -> Returned result
  | exception raised -> (
      let backtrace = Printexc.get_raw_backtrace () in
      let control =
        match raised with
        | Out_of_memory | Stack_overflow | Sys.Break -> true
        | _ -> (
            (* A raising classifier must not replace the original exception or
               its backtrace, so classification failure preserves the
               exception exactly as it arrived. *)
            match is_control_exception raised with
            | control -> control
            | exception _ -> true)
      in
      match control with
      | true -> Printexc.raise_with_backtrace raised backtrace
      | false -> Raised)

type production = {
  console : string -> Io.console_acceptance;
  drains : Drain.t list;
  formatter : Formatter.t option;
}

type output = Production of production | Capture_only of Capture.t

type t = {
  config : Config.t;
  clock : unit -> (Timestamp.t, Io.clock_error) result;
  is_control_exception : exn -> bool;
  output : output;
}

let automatic_console environment =
  match environment with
  | None -> Config.Pretty
  | Some environment -> (
      match String.lowercase_ascii environment with
      | "dev" | "development" -> Config.Pretty
      | _ -> Config.Ndjson)

let create_production config ~console_style ~clock ~console
    ~is_control_exception =
  let policy =
    match Config.console config with
    | Config.Auto -> automatic_console (Config.environment config)
    | (Config.Pretty | Config.Ndjson | Config.Silent) as policy -> policy
  in
  let formatter =
    match policy with
    | Config.Pretty -> Some (Formatter.pretty_line console_style)
    | Config.Ndjson -> Some Formatter.ndjson
    | Config.Silent -> None
    | Config.Auto -> assert false
  in
  {
    config;
    clock;
    is_control_exception;
    output = Production { console; drains = Config.drains config; formatter };
  }

let create_capture config ~clock ~is_control_exception capture =
  { config; clock; is_control_exception; output = Capture_only capture }

let record t kind =
  match t.output with
  | Production _ -> Diagnostics.record kind
  | Capture_only capture -> Capture.record capture kind

let after_production_install t =
  match t.output with
  | Production { formatter = None; drains = []; _ } when Config.enabled t.config
    ->
      Diagnostics.record Diagnostics.No_output
  | Production _ | Capture_only _ -> ()

let admitted config level =
  Config.enabled config && Level.compare level (Config.min_level config) >= 0

let author_payload t = function
  | Message.Text { tag; message } -> Returned (Log.Text { tag; message })
  | Message.Lazy_text { tag; message } -> (
      match
        contain ~is_control_exception:t.is_control_exception (fun () ->
            message ())
      with
      | Returned message -> Returned (Log.Text { tag; message })
      | Raised -> Raised)
  | Message.Free value -> Returned (Log.Free value)
  | Message.Lazy_free make -> (
      match contain ~is_control_exception:t.is_control_exception make with
      | Returned value -> Returned (Log.Free value)
      | Raised -> Raised)
  | Message.Structured (description, value) ->
      Returned (Log.Structured (description, value))
  | Message.Lazy_structured (description, make) -> (
      match contain ~is_control_exception:t.is_control_exception make with
      | Returned value -> Returned (Log.Structured (description, value))
      | Raised -> Raised)

let seal t level timestamp payload =
  Log.Producer.make ~service:(Config.service t.config)
    ?environment:(Config.environment t.config)
    ?version:(Config.version t.config) ~timestamp ~level payload

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
            console output)
      with
      | Returned Io.Accepted -> ()
      | Returned Io.Rejected -> record t Diagnostics.Console_rejected
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
  | Production { console; drains; formatter } ->
      Option.iter
        (fun formatter -> offer_console t console formatter log)
        formatter;
      List.iter (fun drain -> offer_drain t drain log) drains

let emit t level message =
  if admitted t.config level then
    match
      contain ~is_control_exception:t.is_control_exception (fun () ->
          t.clock ())
    with
    | Raised -> record t Diagnostics.Clock_raised
    | Returned (Error Io.Unavailable) -> record t Diagnostics.Clock_unavailable
    | Returned (Ok timestamp) -> (
        match author_payload t message with
        | Raised -> record t Diagnostics.Authoring_raised
        | Returned payload -> deliver t (seal t level timestamp payload))
