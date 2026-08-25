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
  accepting : bool Atomic.t;
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
    accepting = Atomic.make true;
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
    accepting = Atomic.make true;
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

let close t =
  match t.output with
  | Outputs _ -> Atomic.set t.accepting false
  | Capture _ -> ()

let admitted t level =
  Atomic.get t.accepting
  && Config.enabled t.config
  && has_active_route t.output
  && Level.compare level (Config.min_level t.config) >= 0

let empty_fields_fragment =
  Snapshot.Object_accumulator.as_fragment Snapshot.Object_accumulator.empty

let contribution_fragment engine enricher =
  let limits = Config.limits engine.config in
  match
    contain ~is_control_exception:engine.is_control_exception (fun () ->
        let value = (Log_enricher.author enricher) () in
        Value.freeze ~limits value)
  with
  | Raised ->
      record_diagnostic engine Diagnostics.Enricher_raised;
      None
  | Returned (Error _) ->
      record_diagnostic engine Diagnostics.Enricher_invalid;
      None
  | Returned (Ok fragment) ->
      if not (Snapshot.fragment_is_object fragment) then (
        record_diagnostic engine Diagnostics.Enricher_invalid;
        None)
      else if
        Snapshot.fragment_root_has_field_matching Log_envelope.is_reserved_field
          fragment
      then (
        (* A contribution is one ownership unit. A reserved root name
           invalidates that unit rather than allowing its safe siblings to
           change the result. *)
        record_diagnostic engine Diagnostics.Enricher_reserved_field;
        None)
      else Some (Log_enricher.is_authoritative enricher, fragment)

let enrich_fields engine caller =
  let enrichers = Config.enrichers engine.config in
  if enrichers = [] then caller
  else
    let contributions =
      List.filter_map
        (fun enricher -> contribution_fragment engine enricher)
        enrichers
    in
    let limits = Config.limits engine.config in
    match
      contain ~is_control_exception:engine.is_control_exception (fun () ->
          Snapshot.merge_enrichments ~limits ~caller contributions)
    with
    | Raised ->
        record_diagnostic engine Diagnostics.Canonical_freeze_failed;
        caller
    | Returned (Error _) ->
        record_diagnostic engine Diagnostics.Canonical_freeze_failed;
        caller
    | Returned (Ok (merged, conflict)) ->
        if conflict then record_diagnostic engine Diagnostics.Enricher_conflict;
        merged

let evaluate_author t author =
  match
    contain ~is_control_exception:t.is_control_exception (fun () ->
        let materialize materializer =
          let context =
            Snapshot.create_context ~limits:(Config.limits t.config) ()
          in
          match materializer context ~depth:0 with
          | Ok value -> Ok (Snapshot.seal context value)
          | Error _ as error -> error
        in
        match author Message.builder with
        | Message.Text { tag; message } ->
            Ok
              (Log.Producer.Text
                 { tag; message; fields = empty_fields_fragment })
        | Message.Untyped value ->
            Result.map
              (fun value ->
                Log.Producer.Structured { origin = Log.Open; fields = value })
              (Message.materialize_untyped ~limits:(Config.limits t.config)
                 value)
        | Message.Typed (schema, value) ->
            Result.map
              (fun value ->
                Log.Producer.Structured
                  { origin = Log.Declared (Schema.name schema); fields = value })
              (materialize (Schema.freeze_complete schema value)))
  with
  | Raised -> Raised
  | Returned result -> Returned result

let enrich_event t event =
  match Config.enrichers t.config with
  | [] -> event
  | _ -> (
      let fragment =
        match event with
        | Log.Producer.Text { fields; _ }
        | Log.Producer.Structured { fields; _ } ->
            fields
      in
      let fields = enrich_fields t fragment in
      match event with
      | Log.Producer.Text { tag; message; _ } ->
          Log.Producer.Text { tag; message; fields }
      | Log.Producer.Structured { origin; _ } ->
          Log.Producer.Structured { origin; fields })

let create_log t level timestamp ~kind event =
  Log.Producer.make ~service:(Config.service t.config)
    ?environment:(Config.environment t.config)
    ?version:(Config.version t.config) ~timestamp ~level ~kind
    ~limits:(Config.limits t.config) event

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
  match (Atomic.get t.accepting, t.output) with
  | true, Capture capture -> offer_capture t capture log
  | true, Outputs { console; drains; formatter } ->
      Option.iter
        (fun formatter -> format_and_offer_console t console formatter log)
        formatter;
      List.iter (fun drain -> offer_drain t drain log) drains
  | false, (Capture _ | Outputs _) -> ()

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
    | Returned (Ok _) when not (Atomic.get t.accepting) ->
        record_diagnostic t Diagnostics.Runtime_closed
    | Returned (Ok timestamp) -> (
        match evaluate_author t author with
        | Raised -> record_diagnostic t Diagnostics.Message_evaluation_raised
        | Returned (Error _) ->
            record_diagnostic t Diagnostics.Canonical_freeze_failed
        | Returned (Ok body) -> (
            let body = enrich_event t body in
            let kind = Log.Point { correlation } in
            match create_log t level timestamp ~kind body with
            | Ok log -> dispatch t log
            | Error _ -> record_diagnostic t Diagnostics.Canonical_freeze_failed
            ))

type contribution =
  | Contribution of Snapshot.fragment * bool
  | Invalid_contribution of Snapshot.error

(* Authors reserve their callback by incrementing [lifecycle], materialize
   independently, then commit each independent part with a short CAS. Emission
   changes the lifecycle phase to [closing] and returns when an author is
   already active. The last reserved author claims [completing], after which
   all content is stable and can be published exactly once. No participant
   waits for a blocked callback or owns a spin lock. *)

type annotations = { rev : Log.annotation list; count : int; bytes : int }

let accepting = 0
let closing = 1
let completing = 2
let failed = 3
let finished = 4
let phase_bits = 3
let phase_mask = (1 lsl phase_bits) - 1
let author = 1 lsl phase_bits
let max_authors = max_int lsr phase_bits
let lifecycle_phase lifecycle = lifecycle land phase_mask
let lifecycle_authors lifecycle = lifecycle lsr phase_bits

let lifecycle_with_phase lifecycle phase =
  lifecycle land lnot phase_mask lor phase

type wide = {
  name : string;
  origin : Log.structured_origin;
  engine : t option;
  id : string option;
  parent : Log.operation_reference option;
  start_ns : int64 option;
  lifecycle : int Atomic.t;
  body : Snapshot.Object_accumulator.state Atomic.t;
  explicit_level : Level.t option Atomic.t;
  derived_level : Level.t Atomic.t;
  annotations : annotations Atomic.t;
}

type current = Open of wide | Typed of wide * Schema.identity

let empty_annotations = { rev = []; count = 0; bytes = 0 }

let inert_wide () =
  {
    name = "";
    origin = Log.Open;
    engine = None;
    id = None;
    parent = None;
    start_ns = None;
    lifecycle = Atomic.make finished;
    body = Atomic.make Snapshot.Object_accumulator.empty;
    explicit_level = Atomic.make None;
    derived_level = Atomic.make Level.Info;
    annotations = Atomic.make empty_annotations;
  }

let contained_call t callback =
  contain ~is_control_exception:t.is_control_exception callback

let wide_reference wide =
  Option.map
    (fun id -> Log.Producer.operation_reference ~name:wide.name ~id)
    wide.id

let wide_limits wide =
  match wide.engine with
  | None -> Log_limits.default
  | Some engine -> Config.limits engine.config

let current_reference = function
  | Open wide | Typed (wide, _) -> wide_reference wide

let current_wide = function Open wide | Typed (wide, _) -> wide

let own_wide_text t value =
  let length = String.length value in
  if length > Log_limits.max_string_bytes (Config.limits t.config) then
    Error Snapshot.Limit_exceeded
  else Snapshot.own_text value

let wide_parent_valid t = function
  | None -> true
  | Some parent -> (
      match wide_reference parent with
      | None -> true
      | Some reference ->
          let limits = Config.limits t.config in
          let valid value =
            String.length value <= Log_limits.max_string_bytes limits
            && Snapshot.valid_text value
          in
          valid (Log.operation_reference_name reference)
          && valid (Log.operation_reference_id reference))

let create_wide t ?parent ~name ~origin () =
  if
    (not (Atomic.get t.accepting))
    || (not (Config.enabled t.config))
    || not (has_active_route t.output)
  then inert_wide ()
  else if String.trim name = "" then (
    record_diagnostic t Diagnostics.Canonical_freeze_failed;
    inert_wide ())
  else if not (wide_parent_valid t parent) then (
    record_diagnostic t Diagnostics.Canonical_freeze_failed;
    inert_wide ())
  else
    match own_wide_text t name with
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
            match own_wide_text t id with
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
                      lifecycle = Atomic.make accepting;
                      body = Atomic.make Snapshot.Object_accumulator.empty;
                      explicit_level = Atomic.make None;
                      derived_level = Atomic.make Level.Info;
                      annotations = Atomic.make empty_annotations;
                    })))

let merge_body = Snapshot.Object_accumulator.merge

let reject_authoring engine diagnostic =
  record_diagnostic engine diagnostic;
  false

let clear_wide wide =
  Atomic.set wide.body Snapshot.Object_accumulator.empty;
  Atomic.set wide.explicit_level None;
  Atomic.set wide.derived_level Level.Info;
  Atomic.set wide.annotations empty_annotations

let rec release_failed_author wide =
  let before = Atomic.get wide.lifecycle in
  let authors = lifecycle_authors before in
  match lifecycle_phase before with
  | phase when phase = failed && authors > 1 ->
      if not (Atomic.compare_and_set wide.lifecycle before (before - author))
      then release_failed_author wide
  | phase when phase = failed && authors = 1 ->
      if Atomic.compare_and_set wide.lifecycle before failed then
        clear_wide wide
      else release_failed_author wide
  | _ -> ()

let rec fail_reserved_author wide engine diagnostic =
  let before = Atomic.get wide.lifecycle in
  match lifecycle_phase before with
  | phase when phase = accepting || phase = closing ->
      let after = lifecycle_with_phase before failed in
      if Atomic.compare_and_set wide.lifecycle before after then (
        record_diagnostic engine diagnostic;
        release_failed_author wide)
      else fail_reserved_author wide engine diagnostic
  | phase when phase = failed -> release_failed_author wide
  | _ -> ()

let raise_reserved_author_failure wide engine diagnostic raised =
  let backtrace = Printexc.get_raw_backtrace () in
  fail_reserved_author wide engine diagnostic;
  Printexc.raise_with_backtrace raised backtrace

let reserve_authoring wide engine diagnostic =
  let rec reserve () =
    if not (Atomic.get engine.accepting) then
      reject_authoring engine Diagnostics.Runtime_closed
    else
      let before = Atomic.get wide.lifecycle in
      match lifecycle_phase before with
      | phase when phase = accepting ->
          if lifecycle_authors before = max_authors then
            reject_authoring engine Diagnostics.Canonical_freeze_failed
          else if Atomic.compare_and_set wide.lifecycle before (before + author)
          then
            if Atomic.get engine.accepting then true
            else (
              fail_reserved_author wide engine Diagnostics.Runtime_closed;
              false)
          else reserve ()
      | _ -> reject_authoring engine diagnostic
  in
  reserve ()

let complete_wide wide engine =
  Fun.protect
    ~finally:(fun () ->
      clear_wide wide;
      Atomic.set wide.lifecycle finished)
    (fun () ->
      let body =
        Snapshot.Object_accumulator.as_fragment (Atomic.get wide.body)
      in
      let annotations = List.rev (Atomic.get wide.annotations).rev in
      let level =
        match Atomic.get wide.explicit_level with
        | Some level -> level
        | None -> Atomic.get wide.derived_level
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
                    let body = enrich_fields engine body in
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
                          create_log engine level timestamp
                            ~kind:(Log.Wide { operation; annotations })
                            (Log.Producer.Structured
                               { origin = wide.origin; fields = body }))
                    with
                    | Raised | Returned (Error _) ->
                        record_diagnostic engine
                          Diagnostics.Canonical_freeze_failed
                    | Returned (Ok log) -> dispatch engine log)
                | None, _ | _, None ->
                    record_diagnostic engine Diagnostics.Canonical_freeze_failed
              )))

let release_reserved_author wide engine =
  let rec release () =
    let before = Atomic.get wide.lifecycle in
    let authors = lifecycle_authors before in
    match lifecycle_phase before with
    | phase when phase = accepting && authors > 0 ->
        if Atomic.compare_and_set wide.lifecycle before (before - author) then
          true
        else release ()
    | phase when phase = closing && authors > 1 ->
        if Atomic.compare_and_set wide.lifecycle before (before - author) then
          true
        else release ()
    | phase when phase = closing && authors = 1 ->
        if Atomic.compare_and_set wide.lifecycle before completing then (
          complete_wide wide engine;
          true)
        else release ()
    | phase when phase = failed && authors > 0 ->
        release_failed_author wide;
        false
    | _ -> false
  in
  release ()

let derive_level wide level =
  let rec derive () =
    let before = Atomic.get wide.derived_level in
    if Level.compare level before <= 0 then ()
    else if not (Atomic.compare_and_set wide.derived_level before level) then
      derive ()
  in
  derive ()

type body_commit_outcome =
  | Body_committed
  | Body_omitted_needs_release
  | Body_failed_already_released

let commit_body wide engine contribution =
  let rec commit () =
    let before = Atomic.get wide.body in
    match
      contained_call engine (fun () ->
          merge_body ~limits:(Config.limits engine.config) before contribution)
    with
    | Raised ->
        fail_reserved_author wide engine Diagnostics.Message_evaluation_raised;
        Body_failed_already_released
    | Returned (Error Snapshot.Limit_exceeded) ->
        (* A bounded contribution is local to this author.  Preserve the
           already committed body and make the omission observable through
           diagnostics; only malformed or unsafe canonical values fail the
           whole wide lifecycle. *)
        record_diagnostic engine Diagnostics.Canonical_freeze_failed;
        Body_omitted_needs_release
    | Returned (Error _) ->
        fail_reserved_author wide engine Diagnostics.Canonical_freeze_failed;
        Body_failed_already_released
    | Returned (Ok after) ->
        if Atomic.compare_and_set wide.body before after then Body_committed
        else commit ()
  in
  commit ()

let bounded_add left right =
  if right > max_int - left then max_int else left + right

let option_string_length = Option.fold ~none:0 ~some:String.length

let reference_string_length (reference : Log.operation_reference) =
  bounded_add
    (String.length (Log.operation_reference_name reference))
    (String.length (Log.operation_reference_id reference))

let wide_fixed_string_bytes engine wide =
  let config = engine.config in
  let origin_bytes =
    match wide.origin with
    | Log.Open -> 0
    | Log.Declared name -> String.length name
  in
  let operation_bytes =
    bounded_add (String.length wide.name) (option_string_length wide.id)
  in
  let operation_bytes =
    bounded_add operation_bytes
      (Option.fold ~none:0 ~some:reference_string_length wide.parent)
  in
  let config_bytes =
    bounded_add
      (String.length (Config.service config))
      (option_string_length (Config.environment config))
  in
  let config_bytes =
    bounded_add config_bytes (option_string_length (Config.version config))
  in
  bounded_add (bounded_add config_bytes operation_bytes) origin_bytes

let annotation_fits wide engine ~message_bytes before =
  let limits = Config.limits engine.config in
  if before.count >= Log_limits.max_collection_length limits then false
  else if message_bytes > Log_limits.max_string_bytes limits then false
  else
    let body = Snapshot.Object_accumulator.as_fragment (Atomic.get wide.body) in
    let annotation_bytes = bounded_add before.bytes message_bytes in
    let string_bytes =
      bounded_add (wide_fixed_string_bytes engine wide) annotation_bytes
    in
    match
      Snapshot.validate_extension ~limits (Some body) ~nodes:0 ~string_bytes
        ~byte_bytes:0 ~retained_bytes:string_bytes
    with
    | Ok () -> true
    | Error Snapshot.Limit_exceeded -> false
    | Error _ -> false

let commit_annotation wide engine annotation message_bytes =
  let rec commit () =
    let before = Atomic.get wide.annotations in
    if not (annotation_fits wide engine ~message_bytes before) then (
      (* An annotation is an optional contribution.  Dropping an over-limit
         annotation must not transition the wide event to [failed] and clear
         its safe body. *)
      record_diagnostic engine Diagnostics.Canonical_freeze_failed;
      false)
    else
      let after =
        {
          rev = annotation :: before.rev;
          count = before.count + 1;
          bytes = bounded_add before.bytes message_bytes;
        }
      in
      if Atomic.compare_and_set wide.annotations before after then true
      else commit ()
  in
  commit ()

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
            raise_reserved_author_failure wide engine
              Diagnostics.Message_evaluation_raised raised
      in
      match materialized with
      | Raised ->
          fail_reserved_author wide engine Diagnostics.Message_evaluation_raised;
          false
      | Returned (Invalid_contribution Snapshot.Limit_exceeded) ->
          (* Bounded materialization localizes this contribution.  Do not make
             an otherwise safe wide event fail just because one patch cannot
             fit its remaining budget. *)
          record_diagnostic engine Diagnostics.Canonical_freeze_failed;
          ignore (release_reserved_author wide engine : bool);
          false
      | Returned (Invalid_contribution _) ->
          fail_reserved_author wide engine Diagnostics.Canonical_freeze_failed;
          false
      | Returned (Contribution (contribution_body, contribution_has_error)) -> (
          try
            match commit_body wide engine contribution_body with
            | Body_committed ->
                if contribution_has_error then derive_level wide Level.Error;
                release_reserved_author wide engine
            | Body_omitted_needs_release ->
                ignore (release_reserved_author wide engine : bool);
                false
            | Body_failed_already_released -> false
          with raised ->
            raise_reserved_author_failure wide engine
              Diagnostics.Message_evaluation_raised raised))

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
            raise_reserved_author_failure wide engine Diagnostics.Clock_raised
              raised
      in
      match clocked with
      | Raised ->
          fail_reserved_author wide engine Diagnostics.Clock_raised;
          false
      | Returned (Error Io.Unavailable) ->
          fail_reserved_author wide engine Diagnostics.Clock_unavailable;
          false
      | Returned (Ok timestamp) -> (
          let authored =
            match contained_call engine (fun () -> author ()) with
            | result -> result
            | exception raised ->
                raise_reserved_author_failure wide engine
                  Diagnostics.Message_evaluation_raised raised
          in
          match authored with
          | Raised ->
              fail_reserved_author wide engine
                Diagnostics.Message_evaluation_raised;
              false
          | Returned message -> (
              let message_bytes = String.length message in
              let limits = Config.limits engine.config in
              if message_bytes > Log_limits.max_string_bytes limits then (
                record_diagnostic engine Diagnostics.Canonical_freeze_failed;
                ignore (release_reserved_author wide engine : bool);
                false)
              else
                let before = Atomic.get wide.annotations in
                if not (annotation_fits wide engine ~message_bytes before) then (
                  record_diagnostic engine Diagnostics.Canonical_freeze_failed;
                  ignore (release_reserved_author wide engine : bool);
                  false)
                else
                  let materialized =
                    match
                      contained_call engine (fun () ->
                          Snapshot.own_text message)
                    with
                    | result -> result
                    | exception raised ->
                        raise_reserved_author_failure wide engine
                          Diagnostics.Message_evaluation_raised raised
                  in
                  match materialized with
                  | Raised ->
                      fail_reserved_author wide engine
                        Diagnostics.Message_evaluation_raised;
                      false
                  | Returned (Error _) ->
                      fail_reserved_author wide engine
                        Diagnostics.Canonical_freeze_failed;
                      false
                  | Returned (Ok message) -> (
                      match
                        let annotation =
                          Log.Producer.annotation ~timestamp ~level ~message
                        in
                        if
                          commit_annotation wide engine annotation message_bytes
                        then (
                          derive_level wide level;
                          release_reserved_author wide engine)
                        else (
                          ignore (release_reserved_author wide engine : bool);
                          false)
                      with
                      | result -> result
                      | exception raised ->
                          raise_reserved_author_failure wide engine
                            Diagnostics.Message_evaluation_raised raised))))

let set_wide_level wide level =
  match wide.engine with
  | None -> ()
  | Some engine -> (
      if reserve_authoring wide engine Diagnostics.Post_seal_set_level then
        match
          let explicit_level = Some level in
          Atomic.set wide.explicit_level explicit_level;
          ignore (release_reserved_author wide engine : bool)
        with
        | () -> ()
        | exception raised ->
            raise_reserved_author_failure wide engine
              Diagnostics.Message_evaluation_raised raised)

let emit_wide wide =
  match wide.engine with
  | None -> ()
  | Some engine ->
      let rec close () =
        let before = Atomic.get wide.lifecycle in
        match lifecycle_phase before with
        | phase when phase = accepting ->
            let authors = lifecycle_authors before in
            let after =
              if authors = 0 then completing
              else lifecycle_with_phase before closing
            in
            if Atomic.compare_and_set wide.lifecycle before after then (
              if authors = 0 then complete_wide wide engine)
            else close ()
        | _ -> record_diagnostic engine Diagnostics.Post_seal_emit
      in
      close ()
