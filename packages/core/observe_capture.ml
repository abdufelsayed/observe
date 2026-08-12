type contents = { retained_rev : Observe_log.t list; retained_count : int }
type state = Open of contents | Closed of contents

type t = {
  capacity : int;
  state : state Atomic.t;
  diagnostics : Observe_diagnostics.store;
}

let default_capacity = 256

let create ~capacity =
  if capacity <= 0 then
    invalid_arg "Observe.Capture.create: non-positive capacity";
  let contents = { retained_rev = []; retained_count = 0 } in
  {
    capacity;
    state = Atomic.make (Open contents);
    diagnostics = Observe_diagnostics.create_store ();
  }

let contents = function Open contents | Closed contents -> contents
let logs t = List.rev (contents (Atomic.get t.state)).retained_rev
let diagnostics t = Observe_diagnostics.snapshot_store t.diagnostics
let record t = Observe_diagnostics.record_into t.diagnostics

let rec close t =
  match Atomic.get t.state with
  | Closed _ -> ()
  | Open contents as before ->
      if not (Atomic.compare_and_set t.state before (Closed contents)) then
        close t

let rec offer t log =
  match Atomic.get t.state with
  | Closed _ ->
      record t Observe_diagnostics.Capture_closed;
      `Closed
  | Open contents as before ->
      if contents.retained_count < t.capacity then
        let after =
          Open
            {
              retained_rev = log :: contents.retained_rev;
              retained_count = contents.retained_count + 1;
            }
        in
        if Atomic.compare_and_set t.state before after then `Accepted
        else offer t log
      else if Atomic.compare_and_set t.state before before then (
        record t Observe_diagnostics.Capture_overflow;
        `Overflow)
      else offer t log
