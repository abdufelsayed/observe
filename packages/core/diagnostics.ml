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
  | Post_seal_set
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
    Post_seal_set;
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
  | Post_seal_set -> 12
  | Post_seal_set_level -> 13
  | Post_seal_emit -> 14
  | Formatting_failed -> 15
  | Formatting_raised -> 16
  | Console_rejected -> 17
  | Console_raised -> 18
  | Drain_rejected -> 19
  | Drain_raised -> 20
  | Drain_delivery_failed -> 21
  | Capture_overflow -> 22
  | Capture_closed -> 23

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
