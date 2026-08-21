type kind =
  | Not_initialized
  | No_delivery_target
  | Capture_lookup_raised
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
  | Clock_unavailable -> 3
  | Clock_raised -> 4
  | Identity_unavailable -> 5
  | Identity_raised -> 6
  | Monotonic_clock_unavailable -> 7
  | Monotonic_clock_raised -> 8
  | Message_evaluation_raised -> 9
  | Canonical_freeze_failed -> 10
  | Post_seal_set -> 11
  | Post_seal_set_level -> 12
  | Post_seal_emit -> 13
  | Formatting_failed -> 14
  | Formatting_raised -> 15
  | Console_rejected -> 16
  | Console_raised -> 17
  | Drain_rejected -> 18
  | Drain_raised -> 19
  | Drain_delivery_failed -> 20
  | Capture_overflow -> 21
  | Capture_closed -> 22

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
