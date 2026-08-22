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

type output = Outputs of production | Capture of Capture.t

type t = {
  config : Config.t;
  clock : unit -> (Timestamp.t, Io.clock_error) result;
  monotonic_now : unit -> (int64, Io.clock_error) result;
  next_id : unit -> (string, Io.clock_error) result;
  resolve_operation_id : unit -> string option;
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

let create_outputs config ~console_style ~clock ~monotonic_now ~next_id
    ~resolve_operation_id ~console ~is_control_exception =
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
    monotonic_now;
    next_id;
    resolve_operation_id;
    is_control_exception;
    output = Outputs { console; drains = Config.drains config; formatter };
  }

let create_capture config ~clock ~monotonic_now ~next_id ~resolve_operation_id
    ~is_control_exception capture =
  {
    config;
    clock;
    monotonic_now;
    next_id;
    resolve_operation_id;
    is_control_exception;
    output = Capture capture;
  }

let record_diagnostic t kind =
  match t.output with
  | Outputs _ -> Diagnostics.record kind
  | Capture capture -> Capture.record capture kind

let after_install t =
  match t.output with
  | Outputs { formatter = None; drains = []; _ } when Config.enabled t.config ->
      Diagnostics.record Diagnostics.No_delivery_target
  | Outputs _ | Capture _ -> ()

let admitted t level =
  Config.enabled t.config
  && Level.compare level (Config.min_level t.config) >= 0

let evaluate_author t author =
  match
    contain ~is_control_exception:t.is_control_exception (fun () ->
        match author Message.builder with
        | Message.Text { tag; message } ->
            Ok (Log.Producer.Text { tag; message })
        | Message.Value value ->
            Result.map
              (fun value ->
                Log.Producer.Structured { origin = Log.Open; value })
              (Value.freeze value)
        | Message.Untyped value ->
            Result.map
              (fun value ->
                Log.Producer.Structured { origin = Log.Open; value })
              value
        | Message.Typed (schema, value) ->
            Result.map
              (fun value ->
                Log.Producer.Structured
                  { origin = Log.Declared (Schema.name schema); value })
              (Schema.freeze_complete schema value))
  with
  | Raised -> Raised
  | Returned result -> Returned result

let create_log t level timestamp ?correlation_id ?operation body =
  Log.Producer.make ~service:(Config.service t.config)
    ?environment:(Config.environment t.config)
    ?version:(Config.version t.config) ~timestamp ~level ?correlation_id
    ?operation body

let offer_capture t capture log = ignore (Capture.offer capture log)

let format_and_offer_console t console formatter log =
  match
    contain ~is_control_exception:t.is_control_exception (fun () ->
        Formatter.format formatter log)
  with
  | Raised -> record_diagnostic t Diagnostics.Formatting_raised
  | Returned (Error _) -> record_diagnostic t Diagnostics.Formatting_failed
  | Returned (Ok output) -> (
      match
        contain ~is_control_exception:t.is_control_exception (fun () ->
            console output)
      with
      | Returned Io.Accepted -> ()
      | Returned Io.Rejected -> record_diagnostic t Diagnostics.Console_rejected
      | Raised -> record_diagnostic t Diagnostics.Console_raised)

let offer_drain t drain log =
  match
    contain ~is_control_exception:t.is_control_exception (fun () ->
        Drain.offer drain log)
  with
  | Returned Drain.Accepted -> ()
  | Returned Drain.Rejected -> record_diagnostic t Diagnostics.Drain_rejected
  | Raised -> record_diagnostic t Diagnostics.Drain_raised

let dispatch t log =
  match t.output with
  | Capture capture -> offer_capture t capture log
  | Outputs { console; drains; formatter } ->
      Option.iter
        (fun formatter -> format_and_offer_console t console formatter log)
        formatter;
      List.iter (fun drain -> offer_drain t drain log) drains

let emit_point t ?correlation_id level author =
  if admitted t level then
    let correlation_id =
      match correlation_id with
      | Some _ as explicit -> explicit
      | None -> (
          match
            contain ~is_control_exception:t.is_control_exception
              t.resolve_operation_id
          with
          | Raised ->
              record_diagnostic t Diagnostics.Operation_lookup_raised;
              None
          | Returned correlation_id -> correlation_id)
    in
    match
      contain ~is_control_exception:t.is_control_exception (fun () ->
          t.clock ())
    with
    | Raised -> record_diagnostic t Diagnostics.Clock_raised
    | Returned (Error Io.Unavailable) ->
        record_diagnostic t Diagnostics.Clock_unavailable
    | Returned (Ok timestamp) -> (
        match evaluate_author t author with
        | Raised -> record_diagnostic t Diagnostics.Message_evaluation_raised
        | Returned (Error _) ->
            record_diagnostic t Diagnostics.Canonical_freeze_failed
        | Returned (Ok body) -> (
            match create_log t level timestamp ?correlation_id body with
            | Ok log -> dispatch t log
            | Error _ -> record_diagnostic t Diagnostics.Canonical_freeze_failed
            ))

type wide_state = Inert | Available | Authoring | Sealed

(* [closing] is the enrollment gate; [state] serializes the one mutable wide
   snapshot. An author claims [Available] before evaluating caller code and
   rechecks the gate. Emission closes the gate first, then waits for an already
   admitted author to publish [Available] before sealing. Therefore a callback
   either contributes completely before completion or is never evaluated. *)

type contribution =
  | Contribution of Snapshot.fragment * bool
  | Invalid_contribution of Snapshot.error

type wide = {
  name : string;
  origin : Log.structured_origin;
  engine : t option;
  id : string option;
  parent_id : string option;
  start_ns : int64 option;
  mutable body : Snapshot.Object_accumulator.state;
  mutable explicit_level : Level.t option;
  mutable has_error : bool;
  state : wide_state Atomic.t;
  closing : bool Atomic.t;
}

let inert_wide () =
  {
    name = "";
    origin = Log.Open;
    engine = None;
    id = None;
    parent_id = None;
    start_ns = None;
    body = Snapshot.Object_accumulator.empty;
    explicit_level = None;
    has_error = false;
    state = Atomic.make Inert;
    closing = Atomic.make false;
  }

let contained_call t callback =
  contain ~is_control_exception:t.is_control_exception callback

let wide_id wide = wide.id

let own_wide_text ~used value =
  let length = String.length value in
  if length > Snapshot.max_string_bytes - used then
    Error Snapshot.Limit_exceeded
  else Snapshot.own_text value

let create_wide t ?parent ~name ~origin () =
  let routed =
    match t.output with
    | Capture _ -> true
    | Outputs { formatter; drains; _ } ->
        Option.is_some formatter || drains <> []
  in
  if (not (Config.enabled t.config)) || not routed then inert_wide ()
  else if String.trim name = "" then (
    record_diagnostic t Diagnostics.Canonical_freeze_failed;
    inert_wide ())
  else
    match own_wide_text ~used:0 name with
    | Error _ ->
        record_diagnostic t Diagnostics.Canonical_freeze_failed;
        inert_wide ()
    | Ok name -> (
        match contained_call t t.next_id with
        | Raised ->
            record_diagnostic t Diagnostics.Identity_raised;
            inert_wide ()
        | Returned (Error Io.Unavailable) ->
            record_diagnostic t Diagnostics.Identity_unavailable;
            inert_wide ()
        | Returned (Ok id) when String.trim id = "" ->
            record_diagnostic t Diagnostics.Identity_unavailable;
            inert_wide ()
        | Returned (Ok id) -> (
            match own_wide_text ~used:(String.length name) id with
            | Error _ ->
                record_diagnostic t Diagnostics.Identity_unavailable;
                inert_wide ()
            | Ok id -> (
                match contained_call t t.monotonic_now with
                | Raised ->
                    record_diagnostic t Diagnostics.Monotonic_clock_raised;
                    inert_wide ()
                | Returned (Error Io.Unavailable) ->
                    record_diagnostic t Diagnostics.Monotonic_clock_unavailable;
                    inert_wide ()
                | Returned (Ok start_ns) ->
                    let parent_id = Option.bind parent wide_id in
                    {
                      name;
                      origin;
                      engine = Some t;
                      id = Some id;
                      parent_id;
                      start_ns = Some start_ns;
                      body = Snapshot.Object_accumulator.empty;
                      explicit_level = None;
                      has_error = false;
                      state = Atomic.make Available;
                      closing = Atomic.make false;
                    })))

let merge_body = Snapshot.Object_accumulator.merge
let clear_wide_body wide = wide.body <- Snapshot.Object_accumulator.empty

let reject_authoring engine diagnostic =
  record_diagnostic engine diagnostic;
  false

let acquire_authoring wide engine diagnostic =
  let rec acquire () =
    if Atomic.get wide.closing then reject_authoring engine diagnostic
    else
      match Atomic.get wide.state with
      | Inert -> false
      | Sealed -> reject_authoring engine diagnostic
      | Authoring -> acquire ()
      | Available ->
          if Atomic.compare_and_set wide.state Available Authoring then
            if Atomic.get wide.closing then (
              Atomic.set wide.state Available;
              reject_authoring engine diagnostic)
            else true
          else acquire ()
  in
  acquire ()

let finish_authoring wide = Atomic.set wide.state Available

let fail_authoring wide engine diagnostic =
  Atomic.set wide.closing true;
  clear_wide_body wide;
  Atomic.set wide.state Sealed;
  record_diagnostic engine diagnostic

let contribute_wide wide materialize =
  match wide.engine with
  | None -> false
  | Some engine
    when not (acquire_authoring wide engine Diagnostics.Post_seal_set) ->
      false
  | Some engine -> (
      let materialized =
        match contained_call engine materialize with
        | result -> result
        | exception raised ->
            let backtrace = Printexc.get_raw_backtrace () in
            fail_authoring wide engine Diagnostics.Message_evaluation_raised;
            Printexc.raise_with_backtrace raised backtrace
      in
      match materialized with
      | Raised ->
          fail_authoring wide engine Diagnostics.Message_evaluation_raised;
          false
      | Returned (Invalid_contribution _) ->
          fail_authoring wide engine Diagnostics.Canonical_freeze_failed;
          false
      | Returned (Contribution (contribution_body, contribution_has_error)) -> (
          let merged =
            match
              contained_call engine (fun () ->
                  merge_body wide.body contribution_body)
            with
            | result -> result
            | exception raised ->
                let backtrace = Printexc.get_raw_backtrace () in
                fail_authoring wide engine Diagnostics.Message_evaluation_raised;
                Printexc.raise_with_backtrace raised backtrace
          in
          match merged with
          | Raised ->
              fail_authoring wide engine Diagnostics.Message_evaluation_raised;
              false
          | Returned (Error _) ->
              fail_authoring wide engine Diagnostics.Canonical_freeze_failed;
              false
          | Returned (Ok body) ->
              wide.body <- body;
              wide.has_error <- wide.has_error || contribution_has_error;
              finish_authoring wide;
              true))

let set_wide_level wide level =
  match wide.engine with
  | None -> ()
  | Some engine ->
      if acquire_authoring wide engine Diagnostics.Post_seal_set_level then (
        wide.explicit_level <- Some level;
        finish_authoring wide)

let emit_wide wide =
  match wide.engine with
  | None -> ()
  | Some engine ->
      let rec finish_seal () =
        match Atomic.get wide.state with
        | Inert | Sealed -> false
        | Authoring -> finish_seal ()
        | Available ->
            if Atomic.compare_and_set wide.state Available Sealed then true
            else finish_seal ()
      in
      if not (Atomic.compare_and_set wide.closing false true) then
        record_diagnostic engine Diagnostics.Post_seal_emit
      else if finish_seal () then (
        let body = Snapshot.Object_accumulator.as_fragment wide.body in
        clear_wide_body wide;
        let level =
          match wide.explicit_level with
          | Some level -> level
          | None when wide.has_error -> Level.Error
          | None -> Level.Info
        in
        match contained_call engine engine.monotonic_now with
        | Raised -> record_diagnostic engine Diagnostics.Monotonic_clock_raised
        | Returned (Error Io.Unavailable) ->
            record_diagnostic engine Diagnostics.Monotonic_clock_unavailable
        | Returned (Ok end_ns) -> (
            match contained_call engine engine.clock with
            | Raised -> record_diagnostic engine Diagnostics.Clock_raised
            | Returned (Error Io.Unavailable) ->
                record_diagnostic engine Diagnostics.Clock_unavailable
            | Returned (Ok timestamp) -> (
                if admitted engine level then
                  match (wide.id, wide.start_ns) with
                  | Some id, Some start_ns -> (
                      let duration_ns =
                        if Int64.compare end_ns start_ns < 0 then 0L
                        else Int64.sub end_ns start_ns
                      in
                      let operation =
                        Log.Producer.operation ~name:wide.name ~id
                          ?parent_id:wide.parent_id ~duration_ns ()
                      in
                      match
                        contained_call engine (fun () ->
                            create_log engine level timestamp ~operation
                              (Log.Producer.Structured
                                 { origin = wide.origin; value = body }))
                      with
                      | Raised | Returned (Error _) ->
                          record_diagnostic engine
                            Diagnostics.Canonical_freeze_failed
                      | Returned (Ok log) -> dispatch engine log)
                  | None, _ | _, None ->
                      record_diagnostic engine
                        Diagnostics.Canonical_freeze_failed)))
      else ()
