module Observer = Observe.Make (Test_io.IO)

let untyped make (builder : Observe.Logs.builder) = builder.value (make ())

module Raising_get_io = struct
  include Test_io.IO

  let get _ _ = failwith "scope lookup"
end

module Raising_get_observer = Observe.Make (Raising_get_io)

module Raising_operation_get_io = struct
  include Test_io.IO

  let lookups = ref 0

  let get state key =
    incr lookups;
    if !lookups mod 2 = 0 then failwith "operation lookup"
    else Test_io.IO.get state key
end

module Raising_operation_get_observer = Observe.Make (Raising_operation_get_io)
module Inherited_observer = Observe.Make (Test_io.Inherited_io)

let make_observer ?console_style ?now ?monotonic_now ?next_id ?offer_console ()
    =
  let host =
    Test_io.Host.create ?console_style ?now ?monotonic_now ?next_id
      ?offer_console ()
  in
  Observer.create host

let config ?enabled ?console ?min_level ?drains () =
  Test_io.config ?enabled ?console ?min_level ?drains "observer"

let inherited_config ?enabled ?console ?min_level ?drains () =
  Test_io.config ?enabled ?console ?min_level ?drains "inherited"

let init_ok observer config =
  match Observer.init observer config with
  | Ok () -> ()
  | Error Observe.Already_initialized ->
      Alcotest.fail "observer was already initialized"
  | Error Observe.IO_already_registered ->
      Alcotest.fail "a different I/O implementation already owned the route"

let init_inherited_ok observer config =
  match Inherited_observer.init observer config with
  | Ok () -> ()
  | Error Observe.Already_initialized ->
      Alcotest.fail "inherited observer was already initialized"
  | Error Observe.IO_already_registered ->
      Alcotest.fail "a different I/O implementation already owned the route"

let capture_tags capture =
  List.map
    (fun log ->
      match Test_io.text_payload log with
      | Some (tag, message) -> tag ^ ":" ^ message
      | None -> Alcotest.fail "expected a text body")
    (Observe.Capture.logs capture)

let check_capture_tags name expected capture =
  Alcotest.(check string)
    name
    (String.concat "," expected)
    (String.concat "," (capture_tags capture))

let structured_json log =
  match Observe.Log.body log with
  | Observe.Log.Text _ -> Alcotest.fail "expected a structured body"
  | Observe.Log.Structured { value; _ } ->
      Observe.Value.frozen_to_json_string value

let contains value fragment =
  let value_length = String.length value in
  let fragment_length = String.length fragment in
  let rec search offset =
    offset + fragment_length <= value_length
    && (String.equal (String.sub value offset fragment_length) fragment
       || search (offset + 1))
  in
  fragment_length = 0 || search 0

type mapped_value = { mutable text : string }

let mapped_value_t =
  Observe.Type.map Observe.Type.string
    (fun text -> { text })
    (fun value -> value.text)

type mutable_event = {
  payload : bytes;
  numbers : int array;
  reference : string ref;
  deferred : string Lazy.t;
  mapped : mapped_value;
}

let mutable_event_t =
  let open Observe.Type in
  record "mutable_event" (fun payload numbers reference deferred mapped ->
      { payload; numbers; reference; deferred; mapped })
  |+ field "payload" bytes (fun event -> event.payload)
  |+ field "numbers" (array int) (fun event -> event.numbers)
  |+ field "reference" (ref string) (fun event -> event.reference)
  |+ field "deferred" (lazy_t string) (fun event -> event.deferred)
  |+ field "mapped" mapped_value_t (fun event -> event.mapped)
  |> sealr

type mutable_event_builder = {
  typed :
    mutable_event Observe.Schema.patch -> mutable_event Observe.Schema.patch;
}

let mutable_event_schema =
  Observe.Generated_runtime.record_schema mutable_event_t ~builder:(fun _ ->
      { typed = Fun.id })

let wide_lifecycle () =
  let observer = make_observer () in
  let captured = ref None in
  ignore
    (Observer.with_capture observer (config ()) (fun capture ->
         let wide = Observe.Logs.create ~name:"checkout" () in
         Observe.Logs.set wide (fun m ->
             let open Observe.Logs in
             m.untyped
             |+ m.field "cart_id" Observe.Type.string "cart-1"
             |+ m.field "attempt" Observe.Type.int 2
             |> m.seal);
         Observe.Logs.set_level wide Observe.Level.Warn;
         Observe.Logs.emit wide;
         captured := Some capture));
  let capture = Option.get !captured in
  match Observe.Capture.logs capture with
  | [ log ] -> (
      Alcotest.(check bool)
        "wide kind" true
        (Observe.Log.kind log = Observe.Log.Wide);
      Alcotest.(check bool)
        "final level" true
        (Observe.Level.equal Observe.Level.Warn (Observe.Log.level log));
      match Observe.Log.operation log with
      | None -> Alcotest.fail "wide observation has no operation envelope"
      | Some operation ->
          Alcotest.(check string)
            "operation name" "checkout"
            (Observe.Log.operation_name operation);
          Alcotest.(check string)
            "deterministic identifier" "operation-1"
            (Observe.Log.operation_id operation);
          Alcotest.(check int64)
            "monotonic duration" 25L
            (Observe.Log.operation_duration_ns operation))
  | _ -> Alcotest.fail "expected one completed wide observation"

let causal_children_and_correlation () =
  let observer = make_observer () in
  let captured = ref None in
  ignore
    (Observer.with_capture observer (config ()) (fun capture ->
         let parent = Observe.Logs.create ~name:"parent" () in
         Observe.Logs.set parent (fun m ->
             let open Observe.Logs in
             m.untyped
             |+ m.field "parent_only" Observe.Type.string "parent"
             |> m.seal);
         let child =
           Observe.Logs.create_typed ~parent ~name:"typed-child"
             mutable_event_schema
         in
         Observe.Logs.info ~operation:child
           (Test_io.text ~tag:"explicit" "child");
         Observer.with_wide observer parent (fun () ->
             Observe.Logs.info (Test_io.text ~tag:"parent" "before");
             Observer.with_wide observer child (fun () ->
                 Observe.Logs.info (Test_io.text ~tag:"child" "inside"));
             Observe.Logs.info (Test_io.text ~tag:"parent" "after"));
         Observe.Logs.emit child;
         Observe.Logs.emit parent;
         captured := Some capture));
  let logs = Observe.Capture.logs (Option.get !captured) in
  match logs with
  | [ explicit; parent_before; child_inside; parent_after; child; parent ] ->
      List.iter
        (fun (log, expected) ->
          Alcotest.(check (option string))
            "point correlation" (Some expected)
            (Observe.Log.correlation_id log))
        [
          (explicit, "operation-2");
          (parent_before, "operation-1");
          (child_inside, "operation-2");
          (parent_after, "operation-1");
        ];
      let child_operation = Option.get (Observe.Log.operation child) in
      Alcotest.(check string)
        "child has fresh identity" "operation-2"
        (Observe.Log.operation_id child_operation);
      Alcotest.(check (option string))
        "child references parent only" (Some "operation-1")
        (Observe.Log.operation_parent_id child_operation);
      Alcotest.(check string)
        "typed child starts without copied parent fields" "{}"
        (structured_json child);
      let parent_operation = Option.get (Observe.Log.operation parent) in
      Alcotest.(check (option string))
        "parent has no parent" None
        (Observe.Log.operation_parent_id parent_operation);
      Alcotest.(check string)
        "parent payload remains independent" "{\"parent_only\":\"parent\"}"
        (structured_json parent)
  | _ -> Alcotest.fail "expected four point logs and two wide logs"

let managed_execution () =
  Printexc.record_backtrace true;
  let observer = make_observer () in
  let captured = ref None in
  let escaped = Failure "managed" in
  let original_backtrace =
    try failwith "managed-origin"
    with Failure _ -> Printexc.get_raw_backtrace ()
  in
  ignore
    (Observer.with_capture observer (config ()) (fun capture ->
         let success = Observe.Logs.create ~name:"success" () in
         let marker = ref 7 in
         let returned =
           Observer.manage observer success ~error:Observe.Error.exn (fun () ->
               Observe.Logs.info (Test_io.text ~tag:"managed" "success");
               marker)
         in
         Alcotest.(check bool)
           "managed success returns the exact value" true (returned == marker);
         let failure = Observe.Logs.create ~name:"failure" () in
         let caught =
           try
             ignore
               (Observer.manage observer failure ~error:Observe.Error.exn
                  (fun () ->
                    Printexc.raise_with_backtrace escaped original_backtrace));
             None
           with raised -> Some (raised, Printexc.get_raw_backtrace ())
         in
         (match caught with
         | Some (raised, backtrace) ->
             Alcotest.(check bool)
               "managed failure preserves exception identity" true
               (raised == escaped);
             let original =
               Printexc.raw_backtrace_to_string original_backtrace
             in
             let propagated = Printexc.raw_backtrace_to_string backtrace in
             let preserves_origin =
               String.length propagated >= String.length original
               && String.sub propagated 0 (String.length original) = original
             in
             Alcotest.(check bool)
               "managed failure preserves raw backtrace origin" true
               preserves_origin
         | None -> Alcotest.fail "managed failure was swallowed");
         let cancelled = Observe.Logs.create ~name:"cancelled" () in
         Alcotest.check_raises "managed cancellation is preserved"
           Test_io.Direct.Control (fun () ->
             ignore
               (Observer.manage observer cancelled ~error:Observe.Error.exn
                  (fun () -> raise Test_io.Direct.Control)));
         let interpreter_failure =
           Observe.Logs.create ~name:"interpreter-failure" ()
         in
         let original = Failure "application-failure" in
         let raising_interpreter =
           Observe.Error.create (fun _ -> raise Test_io.Direct.Control)
         in
         let propagated =
           try
             ignore
               (Observer.manage observer interpreter_failure
                  ~error:raising_interpreter (fun () -> raise original));
             None
           with raised -> Some raised
         in
         Alcotest.(check bool)
           "interpreter control failure preserves application exception" true
           (match propagated with
           | Some raised -> raised == original
           | None -> false);
         captured := Some capture));
  match Observe.Capture.logs (Option.get !captured) with
  | [ point; success; failure; cancelled ] ->
      Alcotest.(check (option string))
        "managed point uses current identity" (Some "operation-1")
        (Observe.Log.correlation_id point);
      Alcotest.(check bool)
        "success remains info" true
        (Observe.Level.equal Observe.Level.Info (Observe.Log.level success));
      Alcotest.(check bool)
        "ordinary failure derives error" true
        (Observe.Level.equal Observe.Level.Error (Observe.Log.level failure));
      Alcotest.(check bool)
        "failure contains interpreted error" true
        (contains (structured_json failure) "managed");
      Alcotest.(check bool)
        "cancellation infers no error level" true
        (Observe.Level.equal Observe.Level.Info (Observe.Log.level cancelled));
      Alcotest.(check string)
        "cancellation adds no fields" "{}"
        (structured_json cancelled);
      Alcotest.(check int)
        "interpreter failure withholds the managed observation" 1
        (Test_io.diagnostic_count
           (Observe.Capture.diagnostics (Option.get !captured))
           Observe.Diagnostics.Message_evaluation_raised)
  | _ -> Alcotest.fail "expected managed point and three completed wide logs"

let terminal_completion () =
  let observer = make_observer () in
  let captured = ref None in
  ignore
    (Observer.with_capture observer (config ()) (fun capture ->
         let wide = Observe.Logs.create ~name:"stream" () in
         let terminal =
           Observe.Logs.Terminal.create ~error:Observe.Error.exn wide
         in
         Observe.Logs.Terminal.fail terminal
           ~set:(fun m ->
             let open Observe.Logs in
             m.untyped
             |+ m.field "terminal" Observe.Type.string "failure"
             |> m.seal)
           (Failure "stream-failed");
         Observe.Logs.Terminal.complete terminal ();
         Observe.Logs.Terminal.cancel terminal ();
         captured := Some capture));
  match Observe.Capture.logs (Option.get !captured) with
  | [ log ] ->
      Alcotest.(check bool)
        "first terminal action publishes once" true
        (Observe.Level.equal Observe.Level.Error (Observe.Log.level log));
      Alcotest.(check bool)
        "terminal failure is structured" true
        (contains (structured_json log) "stream-failed");
      Alcotest.(check bool)
        "winning terminal facts precede completion" true
        (contains (structured_json log) "\"terminal\":\"failure\"")
  | _ -> Alcotest.fail "terminal actions published more than once"

let operation_lookup_failure () =
  let observer =
    Raising_operation_get_observer.create (Test_io.Host.create ())
  in
  let capture =
    match
      Raising_operation_get_observer.with_capture observer (config ())
        (fun capture ->
          Observe.Logs.info (Test_io.text ~tag:"lookup" "survives");
          capture)
    with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "capture was rejected"
  in
  Alcotest.(check int)
    "operation lookup failure does not withhold point log" 1
    (List.length (Observe.Capture.logs capture));
  Alcotest.(check int)
    "operation lookup failure is captured" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Operation_lookup_raised)

let wide_inert_laziness () =
  let identifiers = ref 0 in
  let monotonic = ref 0 in
  let authored = ref 0 in
  ignore
    (Observer.create
       (Test_io.Host.create
          ~next_id:(fun () ->
            incr identifiers;
            Ok "unexpected")
          ~monotonic_now:(fun () ->
            incr monotonic;
            Ok 0L)
          ()));
  let wide = Observe.Logs.create ~name:"inert" () in
  Observe.Logs.set wide (fun m ->
      incr authored;
      m.seal m.untyped);
  Observe.Logs.set_level wide Observe.Level.Error;
  Observe.Logs.emit wide;
  Alcotest.(check int) "inert creation requests no identifier" 0 !identifiers;
  Alcotest.(check int) "inert creation reads no clock" 0 !monotonic;
  Alcotest.(check int) "inert set is not authored" 0 !authored

let wide_merge_and_seal () =
  let observer = make_observer () in
  let captured = ref None in
  ignore
    (Observer.with_capture observer (config ()) (fun capture ->
         let wide = Observe.Logs.create ~name:"merge" () in
         Observe.Logs.set wide (fun m ->
             let open Observe.Logs in
             m.untyped
             |+ m.field "phase" Observe.Type.string "started"
             |+ m.field "customer"
                  Observe.Type.(
                    record "customer" (fun id plan -> (id, plan))
                    |+ field "id" string fst
                    |+ field "plan" string snd
                    |> sealr)
                  ("customer-1", "free")
             |> m.seal);
         Observe.Logs.set wide (fun m ->
             let open Observe.Logs in
             m.untyped
             |+ m.field "phase" Observe.Type.string "authorized"
             |+ m.field "attempts" Observe.Type.int 2
             |> m.seal);
         Observe.Logs.emit wide;
         let authored_after_seal = ref 0 in
         Observe.Logs.set wide (fun m ->
             incr authored_after_seal;
             m.seal m.untyped);
         Observe.Logs.set_level wide Observe.Level.Error;
         Observe.Logs.emit wide;
         Alcotest.(check int)
           "sealed set is not authored" 0 !authored_after_seal;
         captured := Some capture));
  let capture = Option.get !captured in
  match Observe.Capture.logs capture with
  | [ log ] ->
      Alcotest.(check string)
        "successive fields merge and later scalar replaces"
        "{\"phase\":\"authorized\",\"customer\":{\"id\":\"customer-1\",\"plan\":\"free\"},\"attempts\":2}"
        (structured_json log);
      Alcotest.(check int)
        "post-seal set diagnosed" 1
        (Test_io.diagnostic_count
           (Observe.Capture.diagnostics capture)
           Observe.Diagnostics.Post_seal_set);
      Alcotest.(check int)
        "post-seal level diagnosed" 1
        (Test_io.diagnostic_count
           (Observe.Capture.diagnostics capture)
           Observe.Diagnostics.Post_seal_set_level);
      Alcotest.(check int)
        "repeated emission diagnosed" 1
        (Test_io.diagnostic_count
           (Observe.Capture.diagnostics capture)
           Observe.Diagnostics.Post_seal_emit)
  | _ -> Alcotest.fail "expected exactly one sealed wide observation"

let wide_final_admission () =
  let observer = make_observer () in
  let captured = ref None in
  ignore
    (Observer.with_capture observer (config ~min_level:Observe.Level.Error ())
       (fun capture ->
         let rejected = Observe.Logs.create ~name:"threshold" () in
         Observe.Logs.emit rejected;
         let admitted = Observe.Logs.create ~name:"threshold" () in
         Observe.Logs.set_level admitted Observe.Level.Error;
         Observe.Logs.emit admitted;
         captured := Some capture));
  match Observe.Capture.logs (Option.get !captured) with
  | [ log ] ->
      Alcotest.(check bool)
        "final error level is admitted" true
        (Observe.Level.equal Observe.Level.Error (Observe.Log.level log));
      Alcotest.(check string)
        "empty body remains valid" "{}" (structured_json log)
  | _ -> Alcotest.fail "expected only the final-level admitted observation"

let wide_error_level () =
  Printexc.record_backtrace true;
  let traced_error, traced_backtrace =
    try failwith "traced" with error -> (error, Printexc.get_raw_backtrace ())
  in
  let observer = make_observer () in
  let captured = ref None in
  ignore
    (Observer.with_capture observer (config ~min_level:Observe.Level.Error ())
       (fun capture ->
         let derived = Observe.Logs.create ~name:"derived-error" () in
         Observe.Logs.set derived (fun m ->
             m.error Observe.Error.exn (Failure "boom"));
         Observe.Logs.emit derived;
         let explicit_before = Observe.Logs.create ~name:"explicit-before" () in
         Observe.Logs.set_level explicit_before Observe.Level.Warn;
         Observe.Logs.set explicit_before (fun m ->
             m.error Observe.Error.exn (Failure "before"));
         Observe.Logs.emit explicit_before;
         let explicit_after = Observe.Logs.create ~name:"explicit-after" () in
         Observe.Logs.set explicit_after (fun m ->
             m.error Observe.Error.exn (Failure "after"));
         Observe.Logs.set_level explicit_after Observe.Level.Warn;
         Observe.Logs.emit explicit_after;
         let nested = Observe.Logs.create ~name:"nested-error" () in
         Observe.Logs.set nested (fun m ->
             let open Observe.Logs in
             m.untyped
             |+ m.object_ "failure" (fun n ->
                 n.error Observe.Error.exn (Failure "nested"))
             |> m.seal);
         Observe.Logs.emit nested;
         let traced = Observe.Logs.create ~name:"traced-error" () in
         Observe.Logs.set traced (fun m ->
             m.error Observe.Error.exn ~backtrace:traced_backtrace traced_error);
         Observe.Logs.emit traced;
         captured := Some capture));
  match Observe.Capture.logs (Option.get !captured) with
  | [ direct; nested; traced ] ->
      Alcotest.(check bool)
        "an error derives Error without an explicit level" true
        (Observe.Level.equal Observe.Level.Error (Observe.Log.level direct));
      Alcotest.(check bool)
        "explicit error meaning is structured" true
        (String.starts_with ~prefix:"{\"error\":" (structured_json direct));
      Alcotest.(check bool)
        "nested explicit error also derives Error" true
        (Observe.Level.equal Observe.Level.Error (Observe.Log.level nested));
      Alcotest.(check bool)
        "nested explicit error remains nested data" true
        (String.starts_with ~prefix:"{\"failure\":{\"error\":"
           (structured_json nested));
      Alcotest.(check bool)
        "an explicitly supplied raw backtrace is retained as structured data"
        true
        (contains (structured_json traced) "\"backtrace\":")
  | _ ->
      Alcotest.fail
        "explicit Warn must override derived Error in both call orders, and \
         direct and nested errors must be admitted"

let wide_creation_capability_failures () =
  let identifier_calls = ref 0 in
  let monotonic_calls = ref 0 in
  let authored = ref 0 in
  let host =
    Test_io.Host.create
      ~next_id:(fun () ->
        incr identifier_calls;
        match !identifier_calls with
        | 1 -> Error Observe.IO.Unavailable
        | 2 -> failwith "identity"
        | 3 -> Ok ""
        | _ -> Ok "operation-valid")
      ~monotonic_now:(fun () ->
        incr monotonic_calls;
        Ok (Int64.of_int !monotonic_calls))
      ()
  in
  let observer = Observer.create host in
  let captured = ref None in
  ignore
    (Observer.with_capture observer (config ()) (fun capture ->
         let create_and_touch name =
           let wide = Observe.Logs.create ~name () in
           Observe.Logs.set wide (fun m ->
               incr authored;
               m.seal m.untyped);
           Observe.Logs.emit wide
         in
         create_and_touch "unavailable-identity";
         create_and_touch "raising-identity";
         create_and_touch "empty-identity";
         create_and_touch "valid";
         let invalid_name = Observe.Logs.create ~name:"   " () in
         Observe.Logs.set invalid_name (fun m ->
             incr authored;
             m.seal m.untyped);
         Observe.Logs.emit invalid_name;
         captured := Some capture));
  let capture = Option.get !captured in
  Alcotest.(check int)
    "identity requested only for valid operation names" 4 !identifier_calls;
  Alcotest.(check int)
    "only one successfully created lifecycle is authored" 1 !authored;
  Alcotest.(check int)
    "only active lifecycles read monotonic time" 2 !monotonic_calls;
  Alcotest.(check int)
    "identity unavailability includes empty identifiers" 2
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Identity_unavailable);
  Alcotest.(check int)
    "raising identity provider is contained" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Identity_raised);
  Alcotest.(check int)
    "invalid operation names are withheld" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Canonical_freeze_failed);
  Alcotest.(check int)
    "only the valid operation is published" 1
    (List.length (Observe.Capture.logs capture))

let wide_completion_capability_failures () =
  let monotonic_calls = ref 0 in
  let clock_calls = ref 0 in
  let authored_after_seal = ref 0 in
  let host =
    Test_io.Host.create
      ~monotonic_now:(fun () ->
        incr monotonic_calls;
        match !monotonic_calls with
        | 1 -> Error Observe.IO.Unavailable
        | 2 -> failwith "monotonic start"
        | 3 -> Ok 30L
        | 4 -> Error Observe.IO.Unavailable
        | 5 -> Ok 50L
        | 6 -> failwith "monotonic end"
        | 7 -> Ok 70L
        | _ -> Ok 80L)
      ~now:(fun () ->
        incr clock_calls;
        match !clock_calls with
        | 1 -> Error Observe.IO.Unavailable
        | 2 -> failwith "wall clock"
        | _ -> Ok (Observe.Timestamp.of_unix_ns 90L))
      ()
  in
  let observer = Observer.create host in
  let captured = ref None in
  ignore
    (Observer.with_capture observer (config ()) (fun capture ->
         let create_emit_and_touch name =
           let wide = Observe.Logs.create ~name () in
           Observe.Logs.emit wide;
           Observe.Logs.set wide (fun m ->
               incr authored_after_seal;
               m.seal m.untyped)
         in
         create_emit_and_touch "unavailable-start";
         create_emit_and_touch "raising-start";
         create_emit_and_touch "unavailable-end";
         create_emit_and_touch "raising-end";
         create_emit_and_touch "unavailable-wall";
         create_emit_and_touch "raising-wall";
         create_emit_and_touch "complete";
         captured := Some capture));
  let capture = Option.get !captured in
  Alcotest.(check int)
    "sealed and inert handles do not author later contributions" 0
    !authored_after_seal;
  Alcotest.(check int)
    "monotonic unavailability is diagnosed at start and completion" 2
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Monotonic_clock_unavailable);
  Alcotest.(check int)
    "raising monotonic providers are contained at start and completion" 2
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Monotonic_clock_raised);
  Alcotest.(check int)
    "wide wall-clock unavailability is diagnosed" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Clock_unavailable);
  Alcotest.(check int)
    "raising wide wall clock is contained" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Clock_raised);
  Alcotest.(check int)
    "every lifecycle that reaches completion remains sealed" 5
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Post_seal_set);
  Alcotest.(check int)
    "only a fully completed lifecycle is published" 1
    (List.length (Observe.Capture.logs capture))

let canonical_snapshot () =
  let observer = make_observer () in
  let captured = ref None in
  ignore
    (Observer.with_capture observer (config ()) (fun capture ->
         let payload = Bytes.of_string "before" in
         let numbers = [| 1; 2 |] in
         let reference = ref "reference-before" in
         let deferred_source = ref "deferred-before" in
         let deferred = lazy !deferred_source in
         let mapped = { text = "mapped-before" } in
         Observe.Logs.info (fun m ->
             m.typed mutable_event_schema
               { payload; numbers; reference; deferred; mapped });
         Bytes.set payload 0 'X';
         numbers.(0) <- 99;
         reference := "reference-after";
         deferred_source := "deferred-after";
         mapped.text <- "mapped-after";
         let wide = Observe.Logs.create ~name:"snapshot" () in
         let field = Bytes.of_string "wide-before" in
         Observe.Logs.set wide (fun m ->
             let open Observe.Logs in
             m.untyped |+ m.field "payload" Observe.Type.bytes field |> m.seal);
         Bytes.set field 0 'X';
         Observe.Logs.emit wide;
         captured := Some capture));
  match Observe.Capture.logs (Option.get !captured) with
  | [ point; wide ] ->
      Alcotest.(check string)
        "typed point freezes mutable fields"
        "{\"payload\":\"before\",\"numbers\":[1,2],\"reference\":\"reference-before\",\"deferred\":\"deferred-before\",\"mapped\":\"mapped-before\"}"
        (structured_json point);
      Alcotest.(check string)
        "wide contribution freezes at set" "{\"payload\":\"wide-before\"}"
        (structured_json wide)
  | _ -> Alcotest.fail "expected one point and one wide snapshot"

type 'a boxed_event = { value : 'a }

type 'a boxed_builder = {
  typed :
    'a boxed_event Observe.Schema.patch -> 'a boxed_event Observe.Schema.patch;
}

let typed_box description value (builder : Observe.Logs.builder) =
  let event_t =
    let open Observe.Type in
    record "boxed_event" (fun value -> { value })
    |+ field "value" description (fun event -> event.value)
    |> sealr
  in
  let schema =
    Observe.Generated_runtime.record_schema event_t ~builder:(fun _ ->
        { typed = Fun.id })
  in
  builder.typed schema { value }

type cycle = Next of cycle Lazy.t

let cycle_t =
  Observe.Type.mu (fun self ->
      let open Observe.Type in
      variant "cycle" (fun next -> function Next value -> next value)
      |~ case1
           ~project:(function Next value -> Some value)
           "Next" (lazy_t self)
           (fun value -> Next value)
      |> sealv)

let canonical_bounds_and_failures () =
  let observer = make_observer () in
  let captured = ref None in
  ignore
    (Observer.with_capture observer (config ()) (fun capture ->
         Observe.Logs.info
           (untyped (fun () ->
                Observe.Value.object_
                  [
                    ( "oversized",
                      Observe.Value.string (String.make 1_048_577 'x') );
                  ]));
         Observe.Logs.info
           (untyped (fun () ->
                Observe.Value.object_
                  [
                    ( "oversized-bytes",
                      Observe.Value.embed Observe.Type.bytes
                        (Bytes.make 1_048_577 'x') );
                  ]));
         Observe.Logs.info
           (untyped (fun () ->
                Observe.Value.object_
                  (List.init 1_025 (fun index ->
                       (string_of_int index, Observe.Value.int index)))));
         let rec nested depth value =
           if depth = 0 then value
           else nested (depth - 1) (Observe.Value.object_ [ ("nested", value) ])
         in
         Observe.Logs.info (untyped (fun () -> nested 66 Observe.Value.null));
         Observe.Logs.info
           (untyped (fun () ->
                Observe.Value.list
                  (List.init 1_000 (fun _ ->
                       Observe.Value.list
                         (List.init 100 (fun _ -> Observe.Value.null))))));
         Observe.Logs.info
           (untyped (fun () ->
                let dense_objects =
                  List.init 1_000 (fun _ ->
                      Observe.Value.object_
                        (List.init 95 (fun index ->
                             ( string_of_int index,
                               Observe.Value.float (float_of_int index) ))))
                in
                Observe.Value.object_
                  [
                    ( "bytes",
                      Observe.Value.embed Observe.Type.bytes
                        (Bytes.make 1_000_000 'b') );
                    ("text", Observe.Value.string (String.make 800_000 's'));
                    ("nested", Observe.Value.list dense_objects);
                  ]));
         let rec cyclic = Next (lazy cyclic) in
         Observe.Logs.info (typed_box cycle_t cyclic);
         let rec infinite () = Seq.Cons (1, infinite) in
         Observe.Logs.info
           (typed_box (Observe.Type.seq Observe.Type.int) infinite);
         let raising_record =
           let open Observe.Type in
           record "raising" (fun value -> value)
           |+ field "value" int (fun _ -> failwith "getter")
           |> sealr
         in
         Observe.Logs.info (typed_box raising_record 1);
         Observe.Logs.info
           (typed_box
              (Observe.Type.lazy_t Observe.Type.string)
              (lazy (failwith "lazy")));
         let raising_map =
           Observe.Type.map Observe.Type.string
             (fun _ -> ())
             (fun () -> failwith "map")
         in
         Observe.Logs.info (typed_box raising_map ());
         captured := Some capture));
  let capture = Option.get !captured in
  Alcotest.(check int)
    "unsafe observations are withheld" 0
    (List.length (Observe.Capture.logs capture));
  Alcotest.(check int)
    "each independent canonical bound is diagnosed" 8
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Canonical_freeze_failed);
  Alcotest.(check int)
    "raising projections are contained" 3
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Message_evaluation_raised)

let failed_wide_contribution_seals () =
  let observer = make_observer () in
  let authored_after_failure = ref 0 in
  let captured = ref None in
  ignore
    (Observer.with_capture observer (config ()) (fun capture ->
         let wide = Observe.Logs.create ~name:"failed-contribution" () in
         let opaque = Observe.Type.of_repr Repr.int in
         Observe.Logs.set wide (fun m ->
             let open Observe.Logs in
             m.untyped |+ m.field "opaque" opaque 1 |> m.seal);
         Observe.Logs.set wide (fun m ->
             incr authored_after_failure;
             m.seal m.untyped);
         Observe.Logs.emit wide;
         captured := Some capture));
  let capture = Option.get !captured in
  Alcotest.(check int)
    "failed contribution publishes nothing" 0
    (List.length (Observe.Capture.logs capture));
  Alcotest.(check int)
    "failed contribution seals immediately" 0 !authored_after_failure;
  Alcotest.(check int)
    "failed contribution diagnosed once" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Canonical_freeze_failed);
  Alcotest.(check int)
    "later set is post-seal" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Post_seal_set);
  Alcotest.(check int)
    "later emit is post-seal" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Post_seal_emit)

let aggregate_wide_bound () =
  let observer = make_observer () in
  let authored_after_failure = ref 0 in
  let captured = ref None in
  ignore
    (Observer.with_capture observer (config ()) (fun capture ->
         let wide = Observe.Logs.create ~name:"aggregate-bound" () in
         Observe.Logs.set wide (fun m ->
             let open Observe.Logs in
             List.init 1_024 Fun.id
             |> List.fold_left
                  (fun fields index ->
                    fields
                    |+ m.field (string_of_int index) Observe.Type.int index)
                  m.untyped
             |> m.seal);
         Observe.Logs.set wide (fun m ->
             let open Observe.Logs in
             m.untyped |+ m.field "overflow" Observe.Type.int 1 |> m.seal);
         Observe.Logs.set wide (fun m ->
             incr authored_after_failure;
             m.seal m.untyped);
         Observe.Logs.emit wide;
         captured := Some capture));
  let capture = Option.get !captured in
  Alcotest.(check int)
    "aggregate width failure publishes nothing" 0
    (List.length (Observe.Capture.logs capture));
  Alcotest.(check int)
    "aggregate width is checked after every merge" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Canonical_freeze_failed);
  Alcotest.(check int)
    "aggregate failure seals before later authoring" 0 !authored_after_failure;
  Alcotest.(check int)
    "post-failure set is diagnosed" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Post_seal_set)

let not_initialized () =
  let forced = ref 0 in
  Observe.Logs.info
    (untyped (fun () ->
         incr forced;
         Observe.Value.int 1));
  Alcotest.(check int) "unrouted authoring is not evaluated" 0 !forced;
  Alcotest.(check int)
    "unrouted emission diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Not_initialized)

let inert_create () =
  ignore (make_observer ());
  let owner = make_observer () in
  init_ok owner (config ());
  Observe.Logs.info (Test_io.text ~tag:"owner" "installed")

let repeated_install () =
  let observer = make_observer () in
  init_ok observer (config ());
  Alcotest.(check bool)
    "second install is rejected" true
    (Observer.init observer (config ()) = Error Observe.Already_initialized);
  Alcotest.check_raises "init_exn preserves error"
    (Observe.Init_error Observe.Already_initialized) (fun () ->
      Observer.init_exn observer (config ()))

let io_owner_conflict () =
  let owner = make_observer () in
  let other = make_observer () in
  let capture_claim =
    Observer.with_capture owner (config ()) (fun _ -> Test_io.Direct.return ())
  in
  Alcotest.(check bool)
    "first I/O implementation claims through capture" true
    (capture_claim = Ok ());
  Alcotest.(check bool)
    "different I/O implementation cannot initialize" true
    (Observer.init other (config ()) = Error Observe.IO_already_registered)

let invalid_capacity () =
  let observer = make_observer () in
  let called = ref false in
  let result =
    Observer.with_capture observer (config ()) ~capacity:0 (fun _ ->
        called := true;
        Test_io.Direct.return ())
  in
  Alcotest.(check bool)
    "invalid capacity result" true
    (result = Error (Observe.Invalid_capacity 0));
  Alcotest.(check bool) "callback not entered" false !called;
  init_ok observer (config ())

let threshold_and_authoring () =
  let console = ref [] in
  let observer =
    make_observer
      ~offer_console:(fun output ->
        console := output :: !console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ~min_level:Observe.Level.Warn ());
  let forced = ref 0 in
  Observe.Logs.info (fun m ->
      incr forced;
      m.text ~tag:"threshold" "filtered");
  Observe.Logs.warn (fun m ->
      incr forced;
      m.text ~tag:"threshold" "accepted");
  Alcotest.(check int) "only admitted message authored" 1 !forced;
  Alcotest.(check int)
    "only admitted message delivered" 1 (List.length !console);
  Alcotest.(check char)
    "engine owns console newline" '\n'
    (List.hd !console).[String.length (List.hd !console) - 1]

let clock_unavailable () =
  let console = ref 0 in
  let observer =
    make_observer
      ~now:(fun () -> Error Observe.IO.Unavailable)
      ~offer_console:(fun _ ->
        incr console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ());
  Observe.Logs.info (Test_io.text ~tag:"clock" "missing");
  Alcotest.(check int) "no console delivery" 0 !console;
  Alcotest.(check int)
    "clock diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Clock_unavailable)

let scope_raised () =
  let observer = Raising_get_observer.create (Test_io.Host.create ()) in
  (match
     Raising_get_observer.init observer
       (config ~console:Observe.Config.Silent ())
   with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "raising I/O implementation failed to initialize");
  Observe.Logs.info (Test_io.text ~tag:"scope" "lookup");
  Alcotest.(check int)
    "scope lookup exception diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Capture_lookup_raised)

let formatting_failed () =
  let console = ref 0 in
  let observer =
    make_observer
      ~offer_console:(fun _ ->
        incr console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ());
  Observe.Logs.info (Test_io.text ~tag:"format" (String.make 1 (Char.chr 0xff)));
  Alcotest.(check int) "invalid output not delivered" 0 !console;
  Alcotest.(check int)
    "invalid authoring withheld before formatting" 1
    (Test_io.process_diagnostic_count
       Observe.Diagnostics.Canonical_freeze_failed);
  let raising_json =
    ((fun _ _ -> raise Exit), fun _ -> Error (`Msg "unused decoder"))
  in
  let raising_description =
    Observe.Type.of_repr (Repr.like ~json:raising_json Repr.int)
  in
  Observe.Logs.info
    (untyped (fun () ->
         Observe.Value.object_
           [ ("opaque", Observe.Value.embed raising_description 1) ]));
  Alcotest.(check int) "raised output not delivered" 0 !console;
  Alcotest.(check int)
    "opaque Repr description has no unsafe fallback" 2
    (Test_io.process_diagnostic_count
       Observe.Diagnostics.Canonical_freeze_failed)

let callback_containment () =
  let clock_calls = ref 0 in
  let observer =
    make_observer
      ~now:(fun () ->
        incr clock_calls;
        if !clock_calls = 1 then Ok (Observe.Timestamp.of_unix_ns 1L)
        else failwith "clock")
      ()
  in
  init_ok observer
    (config ~console:Observe.Config.Silent
       ~drains:[ Observe.Drain.create (fun _ -> raise Exit) ]
       ());
  Observe.Logs.info (untyped (fun () -> failwith "authoring"));
  Observe.Logs.info (Test_io.text ~tag:"drain" "raises");
  Alcotest.(check int)
    "authoring diagnosed" 1
    (Test_io.process_diagnostic_count
       Observe.Diagnostics.Message_evaluation_raised);
  Alcotest.(check int)
    "raising clock diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Clock_raised)

let console_and_drains () =
  let accepted = ref 0 in
  let rejected = ref 0 in
  let raised = ref 0 in
  let drains =
    [
      Observe.Drain.create (fun _ ->
          incr accepted;
          Observe.Drain.Accepted);
      Observe.Drain.create (fun _ ->
          incr rejected;
          Observe.Drain.Rejected);
      Observe.Drain.create (fun _ ->
          incr raised;
          failwith "drain");
    ]
  in
  let observer =
    make_observer ~offer_console:(fun _ -> Observe.IO.Rejected) ()
  in
  init_ok observer (config ~drains ());
  Observe.Logs.info (Test_io.text ~tag:"fanout" "message");
  Alcotest.(check int) "accepted drain called" 1 !accepted;
  Alcotest.(check int) "rejected drain called" 1 !rejected;
  Alcotest.(check int) "raising drain called" 1 !raised;
  Alcotest.(check int)
    "console rejection diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Console_rejected);
  Alcotest.(check int)
    "drain rejection diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Drain_rejected);
  Alcotest.(check int)
    "drain exception diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Drain_raised)

let wide_console_and_drains () =
  let console = Buffer.create 256 in
  let first = ref None in
  let second = ref None in
  let drains =
    [
      Observe.Drain.create (fun log ->
          first := Some log;
          Observe.Drain.Accepted);
      Observe.Drain.create (fun _ -> failwith "wide drain");
      Observe.Drain.create (fun log ->
          second := Some log;
          Observe.Drain.Accepted);
    ]
  in
  let monotonic = ref [ 0L; 184_000_000L ] in
  let observer =
    make_observer
      ~now:(fun () -> Ok (Observe.Timestamp.of_unix_ns 37_425_612_000_000L))
      ~monotonic_now:(fun () ->
        match !monotonic with
        | value :: rest ->
            monotonic := rest;
            Ok value
        | [] -> Alcotest.fail "monotonic fixture exhausted")
      ~next_id:(fun () -> Ok "op_checkout")
      ~offer_console:(fun output ->
        Buffer.add_string console output;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ~drains ());
  let wide = Observe.Logs.create ~name:"checkout" () in
  Observe.Logs.set wide (fun m ->
      let open Observe.Logs in
      m.untyped |+ m.field "cart_id" Observe.Type.string "cart-1" |> m.seal);
  Observe.Logs.emit wide;
  let first = Option.get !first in
  let second = Option.get !second in
  Alcotest.(check bool)
    "both drains receive the same completed value" true (first == second);
  let expected =
    match
      Observe.Formatter.format
        (Observe.Formatter.pretty Observe.Formatter.Plain)
        first
    with
    | Ok output -> output ^ "\n"
    | Error _ -> Alcotest.fail "public pretty formatter rejected wide log"
  in
  Alcotest.(check string)
    "automatic console uses public pretty meaning" expected
    (Buffer.contents console);
  Alcotest.(check int)
    "middle drain failure remains branch-local" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Drain_raised)

let console_raised () =
  let observer =
    make_observer ~offer_console:(fun _ -> failwith "console") ()
  in
  init_ok observer (config ());
  Observe.Logs.info (Test_io.text ~tag:"console" "raises");
  Alcotest.(check int)
    "console exception diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Console_raised)

let ansi_console () =
  let output = Buffer.create 128 in
  let observer =
    make_observer ~console_style:Observe.Formatter.Ansi_16
      ~now:(fun () -> Ok (Observe.Timestamp.of_unix_ns 37_425_612_000_000L))
      ~offer_console:(fun value ->
        Buffer.add_string output value;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ());
  Observe.Logs.info (Test_io.text ~tag:"tag" "message");
  Alcotest.(check string)
    "engine selects ANSI formatter"
    "\027[90m10:23:45.612\027[0m \027[1;96mINFO\027[0m \027[1;96m[tag]\027[0m \
     message\n"
    (Buffer.contents output)

let disabled () =
  let console = ref 0 in
  let observer =
    make_observer
      ~offer_console:(fun _ ->
        incr console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ~enabled:false ());
  let forced = ref 0 in
  Observe.Logs.info
    (untyped (fun () ->
         incr forced;
         Observe.Value.int 1));
  Alcotest.(check int) "disabled logging is not authored" 0 !forced;
  Alcotest.(check int) "disabled logging has no output" 0 !console;
  Alcotest.(check int)
    "disabled configuration is not diagnosed as outputless" 0
    (Test_io.process_diagnostic_count Observe.Diagnostics.No_delivery_target)

let no_output () =
  let observer = make_observer () in
  init_ok observer (config ~console:Observe.Config.Silent ());
  Alcotest.(check int)
    "enabled installation with no output is diagnosed" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.No_delivery_target)

let scope_overrides_production () =
  let console = ref 0 in
  let observer =
    make_observer
      ~offer_console:(fun _ ->
        incr console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ());
  let capture =
    match
      Observer.with_capture observer (config ()) (fun capture ->
          Observe.Logs.info (Test_io.text ~tag:"scope" "captured");
          Test_io.Direct.return capture)
    with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "capture was rejected"
  in
  Alcotest.(check int) "scope suppresses console" 0 !console;
  Alcotest.(check int)
    "scope receives message" 1
    (List.length (Observe.Capture.logs capture));
  Observe.Logs.info (Test_io.text ~tag:"production" "restored");
  Alcotest.(check int) "production restored" 1 !console

let nested_capture_precedence () =
  let console = ref 0 in
  let observer =
    make_observer
      ~offer_console:(fun _ ->
        incr console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ());
  let outer, inner =
    match
      Observer.with_capture observer (config ()) (fun outer ->
          Observe.Logs.info (Test_io.text ~tag:"outer" "before-inner");
          let inner =
            match
              Observer.with_capture observer (config ()) (fun inner ->
                  Observe.Logs.info (Test_io.text ~tag:"inner" "inside");
                  Test_io.Direct.return inner)
            with
            | Ok inner -> inner
            | Error _ -> Alcotest.fail "nested capture was rejected"
          in
          Observe.Logs.info (Test_io.text ~tag:"outer" "after-inner");
          Test_io.Direct.return (outer, inner))
    with
    | Ok captures -> captures
    | Error _ -> Alcotest.fail "outer capture was rejected"
  in
  check_capture_tags "outer capture keeps only outer messages"
    [ "outer:before-inner"; "outer:after-inner" ]
    outer;
  check_capture_tags "inner capture takes precedence" [ "inner:inside" ] inner;
  Alcotest.(check int) "nested captures suppress production" 0 !console;
  Observe.Logs.info (Test_io.text ~tag:"production" "after-captures");
  Alcotest.(check int) "production restored after nested captures" 1 !console

let cancellation_capture_close () =
  let context = Test_io.Inherited.create_context () in
  let console = ref 0 in
  let observer =
    Inherited_observer.create
      (Test_io.Inherited_io.create ~context
         ~host:
           (Test_io.Host.create
              ~offer_console:(fun _ ->
                incr console;
                Observe.IO.Accepted)
              ()))
  in
  let capture = ref None in
  let inherited = ref None in
  let cancelled =
    try
      ignore
        (Inherited_observer.with_capture observer (inherited_config ())
           (fun current_capture ->
             capture := Some current_capture;
             inherited := Some (Test_io.Inherited.inherited_context context);
             Observe.Logs.info (Test_io.text ~tag:"cancel" "before-cancel");
             raise Test_io.Inherited.Cancelled));
      false
    with Test_io.Inherited.Cancelled -> true
  in
  Alcotest.(check bool) "native cancellation is preserved" true cancelled;
  Alcotest.(check int)
    "capture protect finalizer runs once" 1
    (Test_io.Inherited.finally_calls context);
  let capture =
    match !capture with
    | Some capture -> capture
    | None -> Alcotest.fail "cancellation did not enter capture"
  in
  check_capture_tags "cancelled capture retains admitted work"
    [ "cancel:before-cancel" ] capture;
  Observe.Logs.info (Test_io.text ~tag:"restored" "before-production-install");
  Alcotest.(check int)
    "binding restored before production install" 1
    (Test_io.process_diagnostic_count Observe.Diagnostics.Not_initialized);
  init_inherited_ok observer (inherited_config ());
  let inherited =
    match !inherited with
    | Some inherited -> inherited
    | None -> Alcotest.fail "inherited context was not retained"
  in
  Test_io.Inherited.with_context context inherited (fun () ->
      Observe.Logs.info (Test_io.text ~tag:"detached" "after-cancel");
      Observe.Logs.info (Test_io.text ~tag:"detached" "after-cancel-again"));
  Alcotest.(check int)
    "closed cancelled capture withholds detached work" 0 !console;
  Alcotest.(check int)
    "each detached offer diagnoses the closed capture" 2
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Capture_closed);
  Observe.Logs.info (Test_io.text ~tag:"production" "restored");
  Alcotest.(check int) "production restored after cancellation" 1 !console

let closed_inherited_tombstone () =
  let context = Test_io.Inherited.create_context () in
  let console = ref 0 in
  let observer =
    Inherited_observer.create
      (Test_io.Inherited_io.create ~context
         ~host:
           (Test_io.Host.create
              ~offer_console:(fun _ ->
                incr console;
                Observe.IO.Accepted)
              ()))
  in
  let inherited = ref None in
  let capture =
    match
      Inherited_observer.with_capture observer (inherited_config ())
        (fun current_capture ->
          inherited := Some (Test_io.Inherited.inherited_context context);
          Observe.Logs.info (Test_io.text ~tag:"capture" "before-detach");
          Test_io.Inherited.return current_capture)
    with
    | Ok capture -> capture
    | Error _ -> Alcotest.fail "capture was rejected"
  in
  init_inherited_ok observer (inherited_config ());
  let inherited =
    match !inherited with
    | Some inherited -> inherited
    | None -> Alcotest.fail "inherited context was not retained"
  in
  Test_io.Inherited.with_context context inherited (fun () ->
      Observe.Logs.info (Test_io.text ~tag:"detached" "closed-scope"));
  Alcotest.(check int)
    "closed inherited scope withholds from production" 0 !console;
  Alcotest.(check int)
    "closed inherited scope is diagnosed" 1
    (Test_io.diagnostic_count
       (Observe.Capture.diagnostics capture)
       Observe.Diagnostics.Capture_closed);
  Observe.Logs.info (Test_io.text ~tag:"production" "after-detach");
  Alcotest.(check int) "root binding restored after detached work" 1 !console

let callback_restoration () =
  let console = ref 0 in
  let observer =
    make_observer
      ~offer_console:(fun _ ->
        incr console;
        Observe.IO.Accepted)
      ()
  in
  init_ok observer (config ());
  Alcotest.check_raises "callback exception preserved" (Failure "callback")
    (fun () ->
      ignore
        (Observer.with_capture observer (config ()) (fun _ ->
             raise (Failure "callback"))));
  Observe.Logs.info (Test_io.text ~tag:"after" "production");
  Alcotest.(check int) "binding restored after exception" 1 !console

let control_exception () =
  let observer = make_observer () in
  init_ok observer
    (config ~console:Observe.Config.Silent
       ~drains:[ Observe.Drain.create (fun _ -> Observe.Drain.Accepted) ]
       ());
  Alcotest.check_raises "I/O control exception preserved" Test_io.Direct.Control
    (fun () ->
      Observe.Logs.info (untyped (fun () -> raise Test_io.Direct.Control)))

let control_backtrace () =
  Printexc.record_backtrace true;
  let original =
    try failwith "raw-backtrace-sentinel"
    with Failure _ -> Printexc.get_raw_backtrace ()
  in
  let original_text = Printexc.raw_backtrace_to_string original in
  Alcotest.(check bool)
    "raw backtrace recording is available" true
    (String.length original_text > 0);
  let observer = make_observer () in
  let actual =
    try
      ignore
        (Observer.with_capture observer
           (config ~console:Observe.Config.Silent ()) (fun _ ->
             Observe.Logs.info
               (untyped (fun () ->
                    Printexc.raise_with_backtrace Test_io.Direct.Control
                      original));
             Test_io.Direct.return ()));
      None
    with Test_io.Direct.Control -> Some (Printexc.get_raw_backtrace ())
  in
  match actual with
  | None -> Alcotest.fail "protected control exception was contained"
  | Some actual ->
      let actual = Printexc.raw_backtrace_to_string actual in
      let preserves_origin =
        String.length actual >= String.length original_text
        && String.sub actual 0 (String.length original_text) = original_text
      in
      Alcotest.(check bool)
        "protected control raw backtrace is preserved" true preserves_origin

let scenario = function
  | "not-initialized" -> not_initialized
  | "wide-lifecycle" -> wide_lifecycle
  | "causal-correlation" -> causal_children_and_correlation
  | "managed-execution" -> managed_execution
  | "terminal-completion" -> terminal_completion
  | "operation-lookup-failure" -> operation_lookup_failure
  | "wide-inert-laziness" -> wide_inert_laziness
  | "wide-merge-and-seal" -> wide_merge_and_seal
  | "wide-final-admission" -> wide_final_admission
  | "wide-error-level" -> wide_error_level
  | "wide-creation-capabilities" -> wide_creation_capability_failures
  | "wide-completion-capabilities" -> wide_completion_capability_failures
  | "canonical-snapshot" -> canonical_snapshot
  | "canonical-bounds" -> canonical_bounds_and_failures
  | "failed-wide-contribution" -> failed_wide_contribution_seals
  | "aggregate-wide-bound" -> aggregate_wide_bound
  | "inert-create" -> inert_create
  | "repeated-install" -> repeated_install
  | "io-owner-conflict" -> io_owner_conflict
  | "invalid-capacity" -> invalid_capacity
  | "threshold-and-authoring" -> threshold_and_authoring
  | "clock-unavailable" -> clock_unavailable
  | "scope-raised" -> scope_raised
  | "formatting-failed" -> formatting_failed
  | "callback-containment" -> callback_containment
  | "console-and-drains" -> console_and_drains
  | "wide-console-and-drains" -> wide_console_and_drains
  | "ANSI-console" -> ansi_console
  | "console-raised" -> console_raised
  | "disabled" -> disabled
  | "no-output" -> no_output
  | "scope-overrides-production" -> scope_overrides_production
  | "nested-capture" -> nested_capture_precedence
  | "cancellation-capture" -> cancellation_capture_close
  | "closed-inherited-tombstone" -> closed_inherited_tombstone
  | "callback-restoration" -> callback_restoration
  | "control-exception" -> control_exception
  | "control-backtrace" -> control_backtrace
  | name -> fun () -> Alcotest.failf "unknown observer scenario: %s" name

let () =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "missing" in
  (scenario name) ()
