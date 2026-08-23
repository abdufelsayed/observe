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
  resolve_operation : unit -> Log.operation_reference option;
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
    ~resolve_operation ~console ~is_control_exception =
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
    resolve_operation;
    is_control_exception;
    output = Outputs { console; drains = Config.drains config; formatter };
  }

let create_capture config ~clock ~monotonic_now ~next_id ~resolve_operation
    ~is_control_exception capture =
  {
    config;
    clock;
    monotonic_now;
    next_id;
    resolve_operation;
    is_control_exception;
    output = Capture capture;
  }

let record_diagnostic t kind =
  match t.output with
  | Outputs _ -> Diagnostics.record kind
  | Capture capture -> Capture.record capture kind

let has_active_route = function
  | Capture _ -> true
  | Outputs { formatter = None; drains = []; _ } -> false
  | Outputs _ -> true

let after_install t =
  match t.output with
  | Outputs { formatter = None; drains = []; _ } when Config.enabled t.config ->
      Diagnostics.record Diagnostics.No_delivery_target
  | Outputs _ | Capture _ -> ()

let admitted t level =
  Config.enabled t.config
  && has_active_route t.output
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

let create_log t level timestamp ?correlation ?operation ?annotations event =
  Log.Producer.make ~service:(Config.service t.config)
    ?environment:(Config.environment t.config)
    ?version:(Config.version t.config) ~timestamp ~level ?correlation ?operation
    ?annotations event

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

let emit_point t ?correlation level author =
  if admitted t level then
    let correlation =
      match correlation with
      | Some _ as explicit -> explicit
      | None -> (
          match
            contain ~is_control_exception:t.is_control_exception
              t.resolve_operation
          with
          | Raised ->
              record_diagnostic t Diagnostics.Operation_lookup_raised;
              None
          | Returned correlation -> correlation)
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
            match create_log t level timestamp ?correlation body with
            | Ok log -> dispatch t log
            | Error _ -> record_diagnostic t Diagnostics.Canonical_freeze_failed
            ))

(* The low lifecycle bits record closing and failure; the remaining bits count
   admitted authors. Reserving an author and closing the lifecycle are competing
   CAS operations on this one word, so a callback is either admitted completely
   before closing or is never evaluated. Admitted callbacks may materialize in
   parallel. [writer] serializes only mutation of the shared accumulator. *)

let lifecycle_closing = 1
let lifecycle_failed = 2
let lifecycle_author = 4

type contribution =
  | Contribution of Snapshot.fragment * bool
  | Invalid_contribution of Snapshot.error

type wide = {
  name : string;
  origin : Log.structured_origin;
  engine : t option;
  id : string option;
  parent : Log.operation_reference option;
  start_ns : int64 option;
  mutable body : Snapshot.Object_accumulator.state;
  mutable explicit_level : Level.t option;
  mutable derived_level : Level.t;
  mutable annotations_rev : Log.annotation list;
  mutable annotation_count : int;
  mutable annotation_bytes : int;
  lifecycle : int Atomic.t;
  writer : bool Atomic.t;
}

type current = Open of wide | Typed of wide * Schema.identity

let inert_wide () =
  {
    name = "";
    origin = Log.Open;
    engine = None;
    id = None;
    parent = None;
    start_ns = None;
    body = Snapshot.Object_accumulator.empty;
    explicit_level = None;
    derived_level = Level.Info;
    annotations_rev = [];
    annotation_count = 0;
    annotation_bytes = 0;
    lifecycle = Atomic.make lifecycle_closing;
    writer = Atomic.make false;
  }

let contained_call t callback =
  contain ~is_control_exception:t.is_control_exception callback

let wide_reference wide =
  Option.map
    (fun id -> Log.Producer.operation_reference ~name:wide.name ~id)
    wide.id

let current_reference = function
  | Open wide | Typed (wide, _) -> wide_reference wide

let own_wide_text ~used value =
  let length = String.length value in
  if length > Snapshot.max_string_bytes - used then
    Error Snapshot.Limit_exceeded
  else Snapshot.own_text value

let create_wide t ?parent ~name ~origin () =
  if (not (Config.enabled t.config)) || not (has_active_route t.output) then
    inert_wide ()
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
                    let parent = Option.bind parent wide_reference in
                    {
                      name;
                      origin;
                      engine = Some t;
                      id = Some id;
                      parent;
                      start_ns = Some start_ns;
                      body = Snapshot.Object_accumulator.empty;
                      explicit_level = None;
                      derived_level = Level.Info;
                      annotations_rev = [];
                      annotation_count = 0;
                      annotation_bytes = 0;
                      lifecycle = Atomic.make 0;
                      writer = Atomic.make false;
                    })))

let merge_body = Snapshot.Object_accumulator.merge

let clear_wide wide =
  wide.body <- Snapshot.Object_accumulator.empty;
  wide.annotations_rev <- [];
  wide.annotation_count <- 0;
  wide.annotation_bytes <- 0

let reject_authoring engine diagnostic =
  record_diagnostic engine diagnostic;
  false

let lifecycle_is_closing state = state land lifecycle_closing <> 0
let lifecycle_is_failed state = state land lifecycle_failed <> 0
let lifecycle_has_authors state = state >= lifecycle_author

let reserve_authoring wide engine diagnostic =
  let rec reserve () =
    let state = Atomic.get wide.lifecycle in
    if lifecycle_is_closing state then reject_authoring engine diagnostic
    else if state > max_int - lifecycle_author then
      reject_authoring engine Diagnostics.Canonical_freeze_failed
    else if
      Atomic.compare_and_set wide.lifecycle state (state + lifecycle_author)
    then true
    else reserve ()
  in
  reserve ()

let release_authoring wide =
  ignore (Atomic.fetch_and_add wide.lifecycle (-lifecycle_author) : int)

let rec acquire_writer wide =
  if Atomic.compare_and_set wide.writer false true then ()
  else acquire_writer wide

let release_writer wide = Atomic.set wide.writer false

let rec mark_failed wide =
  let state = Atomic.get wide.lifecycle in
  if lifecycle_is_failed state then false
  else
    let failed = state lor lifecycle_closing lor lifecycle_failed in
    if Atomic.compare_and_set wide.lifecycle state failed then true
    else mark_failed wide

let fail_authoring wide engine diagnostic =
  let first_failure = mark_failed wide in
  if first_failure then (
    acquire_writer wide;
    clear_wide wide;
    release_writer wide);
  release_authoring wide;
  if first_failure then record_diagnostic engine diagnostic

let fail_authoring_with_writer wide engine diagnostic =
  let first_failure = mark_failed wide in
  if first_failure then clear_wide wide;
  release_writer wide;
  release_authoring wide;
  if first_failure then record_diagnostic engine diagnostic

let authoring_failed wide = lifecycle_is_failed (Atomic.get wide.lifecycle)

let contribute_wide wide materialize =
  match wide.engine with
  | None -> false
  | Some engine
    when not (reserve_authoring wide engine Diagnostics.Post_seal_set) ->
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
          acquire_writer wide;
          if authoring_failed wide then (
            release_writer wide;
            release_authoring wide;
            false)
          else
            let merged =
              match
                contained_call engine (fun () ->
                    merge_body wide.body contribution_body)
              with
              | result -> result
              | exception raised ->
                  let backtrace = Printexc.get_raw_backtrace () in
                  fail_authoring_with_writer wide engine
                    Diagnostics.Message_evaluation_raised;
                  Printexc.raise_with_backtrace raised backtrace
            in
            match merged with
            | Raised ->
                fail_authoring_with_writer wide engine
                  Diagnostics.Message_evaluation_raised;
                false
            | Returned (Error _) ->
                fail_authoring_with_writer wide engine
                  Diagnostics.Canonical_freeze_failed;
                false
            | Returned (Ok body) ->
                wide.body <- body;
                if contribution_has_error then
                  wide.derived_level <-
                    (if Level.compare Level.Error wide.derived_level > 0 then
                       Level.Error
                     else wide.derived_level);
                release_writer wide;
                release_authoring wide;
                true))

let annotate_wide wide level author =
  match wide.engine with
  | None -> false
  | Some engine
    when not (reserve_authoring wide engine Diagnostics.Post_seal_annotate) ->
      false
  | Some engine -> (
      let clocked =
        match contained_call engine engine.clock with
        | result -> result
        | exception raised ->
            let backtrace = Printexc.get_raw_backtrace () in
            fail_authoring wide engine Diagnostics.Clock_raised;
            Printexc.raise_with_backtrace raised backtrace
      in
      match clocked with
      | Raised ->
          fail_authoring wide engine Diagnostics.Clock_raised;
          false
      | Returned (Error Io.Unavailable) ->
          fail_authoring wide engine Diagnostics.Clock_unavailable;
          false
      | Returned (Ok timestamp) -> (
          let materialized =
            match
              contained_call engine (fun () -> Snapshot.own_text (author ()))
            with
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
          | Returned (Error _) ->
              fail_authoring wide engine Diagnostics.Canonical_freeze_failed;
              false
          | Returned (Ok message) ->
              acquire_writer wide;
              let message_bytes = String.length message in
              if
                authoring_failed wide
                || wide.annotation_count >= Snapshot.width_limit
                || message_bytes
                   > Snapshot.max_string_bytes - wide.annotation_bytes
              then (
                if authoring_failed wide then (
                  release_writer wide;
                  release_authoring wide)
                else
                  fail_authoring_with_writer wide engine
                    Diagnostics.Canonical_freeze_failed;
                false)
              else (
                wide.annotations_rev <-
                  Log.Producer.annotation ~timestamp ~level ~message
                  :: wide.annotations_rev;
                wide.annotation_count <- wide.annotation_count + 1;
                wide.annotation_bytes <- wide.annotation_bytes + message_bytes;
                if Level.compare level wide.derived_level > 0 then
                  wide.derived_level <- level;
                release_writer wide;
                release_authoring wide;
                true)))

let set_wide_level wide level =
  match wide.engine with
  | None -> ()
  | Some engine ->
      if reserve_authoring wide engine Diagnostics.Post_seal_set_level then (
        acquire_writer wide;
        if not (authoring_failed wide) then wide.explicit_level <- Some level;
        release_writer wide;
        release_authoring wide)

let rec close_lifecycle wide =
  let state = Atomic.get wide.lifecycle in
  if lifecycle_is_closing state then false
  else if
    Atomic.compare_and_set wide.lifecycle state (state lor lifecycle_closing)
  then true
  else close_lifecycle wide

let rec wait_for_authors wide =
  if lifecycle_has_authors (Atomic.get wide.lifecycle) then
    wait_for_authors wide

let emit_wide wide =
  match wide.engine with
  | None -> ()
  | Some engine ->
      if not (close_lifecycle wide) then
        record_diagnostic engine Diagnostics.Post_seal_emit
      else (
        wait_for_authors wide;
        if not (authoring_failed wide) then (
          let body = Snapshot.Object_accumulator.as_fragment wide.body in
          let annotations = List.rev wide.annotations_rev in
          clear_wide wide;
          let level =
            match wide.explicit_level with
            | Some level -> level
            | None -> wide.derived_level
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
                  if admitted engine level then
                    match (wide.id, wide.start_ns) with
                    | Some id, Some start_ns -> (
                        let duration_ns =
                          if Int64.compare end_ns start_ns < 0 then 0L
                          else Int64.sub end_ns start_ns
                        in
                        let operation =
                          Log.Producer.operation ~name:wide.name ~id
                            ?parent:wide.parent ~duration_ns ()
                        in
                        match
                          contained_call engine (fun () ->
                              create_log engine level timestamp ~operation
                                ~annotations
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
        else ())
