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

let create_outputs config ~console_style ~clock ~monotonic_now ~next_id ~console
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
    monotonic_now;
    next_id;
    is_control_exception;
    output = Outputs { console; drains = Config.drains config; formatter };
  }

let create_capture config ~clock ~monotonic_now ~next_id ~is_control_exception
    capture =
  {
    config;
    clock;
    monotonic_now;
    next_id;
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

let admitted config level =
  Config.enabled config && Level.compare level (Config.min_level config) >= 0

let evaluate_author t author =
  match
    contain ~is_control_exception:t.is_control_exception (fun () ->
        match author Message.builder with
        | Message.Text { tag; message } -> Ok (Log.Text { tag; message })
        | Message.Untyped value ->
            Result.map
              (fun value -> Log.Structured { origin = Log.Open; value })
              (Value.freeze value)
        | Message.Typed (schema, value) ->
            Result.map
              (fun value ->
                Log.Structured
                  { origin = Log.Declared (Schema.name schema); value })
              (Schema.freeze_complete schema value))
  with
  | Raised -> Raised
  | Returned result -> Returned result

let create_log t level timestamp ?operation body =
  Log.Producer.make ~service:(Config.service t.config)
    ?environment:(Config.environment t.config)
    ?version:(Config.version t.config) ~timestamp ~level ?operation body

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

let emit_point t level author =
  if admitted t.config level then
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
            match create_log t level timestamp body with
            | Ok log -> dispatch t log
            | Error _ -> record_diagnostic t Diagnostics.Canonical_freeze_failed
            ))

type wide_active = {
  body : Snapshot.t;
  explicit_level : Level.t option;
  has_error : bool;
  id : string;
  start_ns : int64;
}

type wide_state = Inert | Active of wide_active | Sealed
type contribution = { body : Snapshot.t; has_error : bool }

type wide = {
  name : string;
  origin : Log.structured_origin;
  engine : t option;
  state : wide_state Atomic.t;
}

let inert_wide () =
  { name = ""; origin = Log.Open; engine = None; state = Atomic.make Inert }

let contained_call t callback =
  contain ~is_control_exception:t.is_control_exception callback

let create_wide t ~name ~origin =
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
    let context = Snapshot.create_context () in
    match Snapshot.copy_text context ~depth:0 name with
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
            match Snapshot.copy_text context ~depth:0 id with
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
                    {
                      name;
                      origin;
                      engine = Some t;
                      state =
                        Atomic.make
                          (Active
                             {
                               body = Snapshot.Object [];
                               explicit_level = None;
                               has_error = false;
                               id;
                               start_ns;
                             });
                    })))

let merge_body = Snapshot.merge_object

let abort_contribution wide engine diagnostic =
  let rec abort () =
    match Atomic.get wide.state with
    | Inert -> ()
    | Sealed -> record_diagnostic engine Diagnostics.Post_seal_set
    | Active _ as before ->
        if Atomic.compare_and_set wide.state before Sealed then
          record_diagnostic engine diagnostic
        else abort ()
  in
  abort ()

let contribute_wide wide materialize =
  match (wide.engine, Atomic.get wide.state) with
  | None, _ | Some _, Inert -> ()
  | Some engine, Sealed -> record_diagnostic engine Diagnostics.Post_seal_set
  | Some engine, Active _ -> (
      match contained_call engine materialize with
      | Raised ->
          abort_contribution wide engine Diagnostics.Message_evaluation_raised
      | Returned (Error _) ->
          abort_contribution wide engine Diagnostics.Canonical_freeze_failed
      | Returned (Ok contribution) ->
          let rec apply () =
            match Atomic.get wide.state with
            | Inert -> ()
            | Sealed -> record_diagnostic engine Diagnostics.Post_seal_set
            | Active active as before -> (
                match
                  contained_call engine (fun () ->
                      merge_body active.body contribution.body)
                with
                | Raised ->
                    abort_contribution wide engine
                      Diagnostics.Message_evaluation_raised
                | Returned (Error _) ->
                    abort_contribution wide engine
                      Diagnostics.Canonical_freeze_failed
                | Returned (Ok body) ->
                    let after =
                      Active
                        {
                          active with
                          body;
                          has_error = active.has_error || contribution.has_error;
                        }
                    in
                    if not (Atomic.compare_and_set wide.state before after) then
                      apply ())
          in
          apply ())

let rec set_wide_level wide level =
  match Atomic.get wide.state with
  | Inert -> ()
  | Sealed ->
      Option.iter
        (fun engine -> record_diagnostic engine Diagnostics.Post_seal_set_level)
        wide.engine
  | Active active as before ->
      let after = Active { active with explicit_level = Some level } in
      if not (Atomic.compare_and_set wide.state before after) then
        set_wide_level wide level

let emit_wide wide =
  match wide.engine with
  | None -> ()
  | Some engine ->
      let rec seal () =
        match Atomic.get wide.state with
        | Inert -> None
        | Sealed ->
            record_diagnostic engine Diagnostics.Post_seal_emit;
            None
        | Active active as before ->
            if Atomic.compare_and_set wide.state before Sealed then Some active
            else seal ()
      in
      Option.iter
        (fun active ->
          let level =
            match active.explicit_level with
            | Some level -> level
            | None when active.has_error -> Level.Error
            | None -> Level.Info
          in
          match contained_call engine engine.monotonic_now with
          | Raised ->
              record_diagnostic engine Diagnostics.Monotonic_clock_raised
          | Returned (Error Io.Unavailable) ->
              record_diagnostic engine Diagnostics.Monotonic_clock_unavailable
          | Returned (Ok end_ns) -> (
              match contained_call engine engine.clock with
              | Raised -> record_diagnostic engine Diagnostics.Clock_raised
              | Returned (Error Io.Unavailable) ->
                  record_diagnostic engine Diagnostics.Clock_unavailable
              | Returned (Ok timestamp) -> (
                  if admitted engine.config level then
                    let duration_ns =
                      if Int64.compare end_ns active.start_ns < 0 then 0L
                      else Int64.sub end_ns active.start_ns
                    in
                    let operation =
                      Log.Producer.operation ~name:wide.name ~id:active.id
                        ~duration_ns ()
                    in
                    match
                      contained_call engine (fun () ->
                          create_log engine level timestamp ~operation
                            (Log.Structured
                               { origin = wide.origin; value = active.body }))
                    with
                    | Raised | Returned (Error _) ->
                        record_diagnostic engine
                          Diagnostics.Canonical_freeze_failed
                    | Returned (Ok log) -> dispatch engine log)))
        (seal ())
