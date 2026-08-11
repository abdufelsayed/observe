type kind =
  | Not_initialized
  | No_output
  | Scope_raised
  | Clock_unavailable
  | Clock_raised
  | Authoring_raised
  | Formatting_failed
  | Formatting_raised
  | Terminal_rejected
  | Terminal_raised
  | Drain_rejected
  | Drain_raised
  | Capture_overflow
  | Capture_closed

type entry = { kind : kind; count : int }
type store = int Atomic.t array

let kinds =
  [|
    Not_initialized;
    No_output;
    Scope_raised;
    Clock_unavailable;
    Clock_raised;
    Authoring_raised;
    Formatting_failed;
    Formatting_raised;
    Terminal_rejected;
    Terminal_raised;
    Drain_rejected;
    Drain_raised;
    Capture_overflow;
    Capture_closed;
  |]

let index = function
  | Not_initialized -> 0
  | No_output -> 1
  | Scope_raised -> 2
  | Clock_unavailable -> 3
  | Clock_raised -> 4
  | Authoring_raised -> 5
  | Formatting_failed -> 6
  | Formatting_raised -> 7
  | Terminal_rejected -> 8
  | Terminal_raised -> 9
  | Drain_rejected -> 10
  | Drain_raised -> 11
  | Capture_overflow -> 12
  | Capture_closed -> 13

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
