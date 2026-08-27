type kind =
  | Not_initialized
  | No_delivery_target
  | Capture_lookup_raised
  | Operation_lookup_raised
  | Clock_unavailable
  | Clock_raised
  | Identity_unavailable
  | Identity_raised
  | Monotonic_clock_unavailable
  | Monotonic_clock_raised
  | Message_evaluation_raised
  | Canonical_freeze_failed
  | Enricher_raised
  | Enricher_invalid
  | Enricher_conflict
  | Enricher_reserved_field
  | Post_seal_set
  | Post_seal_annotate
  | Post_seal_set_level
  | Post_seal_emit
  | Formatting_failed
  | Formatting_raised
  | Console_rejected
  | Console_raised
  | Drain_rejected
  | Drain_raised
  | Drain_delivery_failed
  | Capture_overflow
  | Capture_closed
  | Runtime_closed
  | Redaction_failed
  | Redaction_conflict
  | Drain_redaction_failed
  | Sampling_discarded
  | Sampling_source_raised
  | Sampling_source_invalid
  | Retention_raised
  | Routing_raised

type entry = { kind : kind; count : int }
type store = int Atomic.t array

let kinds =
  [|
    Not_initialized;
    No_delivery_target;
    Capture_lookup_raised;
    Operation_lookup_raised;
    Clock_unavailable;
    Clock_raised;
    Identity_unavailable;
    Identity_raised;
    Monotonic_clock_unavailable;
    Monotonic_clock_raised;
    Message_evaluation_raised;
    Canonical_freeze_failed;
    Enricher_raised;
    Enricher_invalid;
    Enricher_conflict;
    Enricher_reserved_field;
    Post_seal_set;
    Post_seal_annotate;
    Post_seal_set_level;
    Post_seal_emit;
    Formatting_failed;
    Formatting_raised;
    Console_rejected;
    Console_raised;
    Drain_rejected;
    Drain_raised;
    Drain_delivery_failed;
    Capture_overflow;
    Capture_closed;
    Runtime_closed;
    Redaction_failed;
    Redaction_conflict;
    Drain_redaction_failed;
    Sampling_discarded;
    Sampling_source_raised;
    Sampling_source_invalid;
    Retention_raised;
    Routing_raised;
  |]

let index = function
  | Not_initialized -> 0
  | No_delivery_target -> 1
  | Capture_lookup_raised -> 2
  | Operation_lookup_raised -> 3
  | Clock_unavailable -> 4
  | Clock_raised -> 5
  | Identity_unavailable -> 6
  | Identity_raised -> 7
  | Monotonic_clock_unavailable -> 8
  | Monotonic_clock_raised -> 9
  | Message_evaluation_raised -> 10
  | Canonical_freeze_failed -> 11
  | Enricher_raised -> 12
  | Enricher_invalid -> 13
  | Enricher_conflict -> 14
  | Enricher_reserved_field -> 15
  | Post_seal_set -> 16
  | Post_seal_annotate -> 17
  | Post_seal_set_level -> 18
  | Post_seal_emit -> 19
  | Formatting_failed -> 20
  | Formatting_raised -> 21
  | Console_rejected -> 22
  | Console_raised -> 23
  | Drain_rejected -> 24
  | Drain_raised -> 25
  | Drain_delivery_failed -> 26
  | Capture_overflow -> 27
  | Capture_closed -> 28
  | Runtime_closed -> 29
  | Redaction_failed -> 30
  | Redaction_conflict -> 31
  | Drain_redaction_failed -> 32
  | Sampling_discarded -> 33
  | Sampling_source_raised -> 34
  | Sampling_source_invalid -> 35
  | Retention_raised -> 36
  | Routing_raised -> 37

let create_store () = Array.init (Array.length kinds) (fun _ -> Atomic.make 0)

let rec increment counter =
  let before = Atomic.get counter in
  if before < max_int then
    let after = before + 1 in
    if not (Atomic.compare_and_set counter before after) then increment counter

let record_into counters kind = increment counters.(index kind)

let snapshot_store counters =
  let entries = ref [] in
  Array.iteri
    (fun index kind ->
      let count = Atomic.get counters.(index) in
      if count <> 0 then entries := { kind; count } :: !entries)
    kinds;
  List.rev !entries

let process = create_store ()
let record kind = record_into process kind
let snapshot () = snapshot_store process
