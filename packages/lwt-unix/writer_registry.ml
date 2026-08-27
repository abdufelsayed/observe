type error = Closed
type identity = { name : string }

module Delivery_facts = struct
  type t = No_problem | Rejected | Delivery_lost | Rejected_and_lost
end

type delivery_facts = Delivery_facts.t
type integration_error = Closed | Invalid_label

type problem =
  | Rejected of { output : string }
  | Delivery_lost of { output : string }
  | Destination_failed of { output : string }
  | Timed_out of { output : string }
  | Cancelled of { output : string }

type report = { complete : bool; problems : problem list }

let empty_report = { complete = true; problems = [] }
let report_complete report = report.complete
let report_problems report = report.problems

type hook = {
  identity : identity;
  facts : unit -> delivery_facts;
  flush : unit -> unit Lwt.t;
  shutdown : unit -> unit Lwt.t;
}

type settlement = Pending | Settled of (unit, exn) result
type operation_outcome = (report, exn) result

type operation = {
  mutex : Mutex.t;
  hooks : hook array;
  settlements : settlement array;
  mutable remaining : int;
  report : report Lwt.t;
  wakener : report Lwt.u;
  on_complete : operation_outcome -> unit;
}

type status = Open | Closing of operation | Settled of operation_outcome
type t = { mutex : Mutex.t; mutable hooks : hook list; mutable status : status }

let with_mutex mutex callback =
  Mutex.lock mutex;
  match callback () with
  | result ->
      Mutex.unlock mutex;
      result
  | exception exn ->
      Mutex.unlock mutex;
      raise exn

let with_lock t callback = with_mutex t.mutex callback
let create () = { mutex = Mutex.create (); hooks = []; status = Open }
let next_identity = Atomic.make 0

let create_identity ?name () =
  let number = Atomic.fetch_and_add next_identity 1 + 1 in
  let name =
    match name with
    | Some name when String.length name > 0 ->
        String.sub name 0 (min 128 (String.length name))
    | Some _ | None -> Printf.sprintf "registered-output-%d" number
  in
  { name }

let identity_name identity = identity.name

let valid_label label =
  let length = String.length label in
  length > 0
  && length <= 128
  &&
  let rec loop index =
    index = length
    ||
    let code = Char.code (String.unsafe_get label index) in
    code >= 0x20 && code <= 0x7e && loop (index + 1)
  in
  loop 0

let register t ~identity ~flush ~shutdown : (unit, error) result =
  with_lock t (fun () ->
      match t.status with
      | Open ->
          t.hooks <-
            {
              identity;
              facts = (fun () -> Delivery_facts.No_problem);
              flush;
              shutdown;
            }
            :: t.hooks;
          Ok ()
      | Closing _ | Settled _ -> Error (Closed : error))

let register_integration t ~label ~facts ~flush ~shutdown :
    (unit, integration_error) result =
  if not (valid_label label) then Error Invalid_label
  else
    with_lock t (fun () ->
        match t.status with
        | Open ->
            t.hooks <-
              { identity = { name = label }; facts; flush; shutdown } :: t.hooks;
            Ok ()
        | Closing _ | Settled _ -> Error Closed)

let problem_of_outcome name = function
  | Ok () -> None
  | Error Lwt.Canceled -> Some (Cancelled { output = name })
  | Error _ -> Some (Destination_failed { output = name })

let problems_of_facts name = function
  | Delivery_facts.No_problem -> []
  | Delivery_facts.Rejected -> [ Rejected { output = name } ]
  | Delivery_facts.Delivery_lost -> [ Delivery_lost { output = name } ]
  | Delivery_facts.Rejected_and_lost ->
      [ Rejected { output = name }; Delivery_lost { output = name } ]

let preserve_fatal = function
  | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> raise exn
  | _ -> ()

let report_of_settlements ~complete hooks settlements =
  let problems =
    let rec collect index problems =
      if index < 0 then problems
      else
        let hook = hooks.(index) in
        let output = hook.identity.name in
        let fact_problems, facts_failed =
          try (problems_of_facts output (hook.facts ()), false)
          with exn ->
            preserve_fatal exn;
            ([ Destination_failed { output } ], true)
        in
        let settlement_problems =
          match settlements.(index) with
          | Pending -> []
          | Settled (Error exn) -> (
              preserve_fatal exn;
              match problem_of_outcome output (Error exn) with
              | None -> []
              | Some (Destination_failed _) when facts_failed -> []
              | Some problem -> [ problem ])
          | Settled (Ok () as outcome) -> (
              match problem_of_outcome output outcome with
              | None -> []
              | Some (Destination_failed _) when facts_failed -> []
              | Some problem -> [ problem ])
        in
        collect (index - 1) (settlement_problems @ fact_problems @ problems)
    in
    collect (Array.length hooks - 1) []
  in
  { complete = complete && problems = []; problems }

let snapshot (operation : operation) =
  let complete, settlements =
    with_mutex operation.mutex (fun () ->
        (operation.remaining = 0, Array.copy operation.settlements))
  in
  let report = report_of_settlements ~complete operation.hooks settlements in
  let report =
    if complete then report
    else
      let problems =
        let pending = ref [] in
        Array.iteri
          (fun index (settlement : settlement) ->
            match settlement with
            | Settled _ -> ()
            | Pending ->
                pending :=
                  Timed_out { output = operation.hooks.(index).identity.name }
                  :: !pending)
          settlements;
        report.problems @ List.rev !pending
      in
      { report with problems }
  in
  report

let finish (operation : operation) index outcome =
  let settlements =
    with_mutex operation.mutex (fun () ->
        match operation.settlements.(index) with
        | Settled _ -> None
        | Pending ->
            operation.settlements.(index) <- Settled outcome;
            operation.remaining <- operation.remaining - 1;
            if operation.remaining = 0 then Some operation.settlements else None)
  in
  match settlements with
  | None -> ()
  | Some settlements -> (
      let outcome =
        try
          Ok (report_of_settlements ~complete:true operation.hooks settlements)
        with (Out_of_memory | Stack_overflow | Sys.Break) as exn -> Error exn
      in
      match outcome with
      | Ok report ->
          Fun.protect
            ~finally:(fun () -> Lwt.wakeup_later operation.wakener report)
            (fun () -> operation.on_complete outcome)
      | Error exn ->
          Fun.protect
            ~finally:(fun () -> Lwt.wakeup_later_exn operation.wakener exn)
            (fun () -> operation.on_complete outcome))

let make_operation ?(on_complete = fun _ -> ()) hooks callback =
  let hooks = Array.of_list hooks in
  let report, wakener = Lwt.wait () in
  ( {
      mutex = Mutex.create ();
      hooks;
      settlements = Array.make (Array.length hooks) Pending;
      remaining = Array.length hooks;
      report;
      wakener;
      on_complete;
    },
    callback )

let start_operation (operation : operation) callback =
  if Array.length operation.hooks = 0 then
    (* There is no hook index to settle; make an empty operation complete. *)
    let report = { complete = true; problems = [] } in
    Fun.protect
      ~finally:(fun () -> Lwt.wakeup_later operation.wakener report)
      (fun () -> operation.on_complete (Ok report))
  else
    Array.iteri
      (fun index hook ->
        match callback hook () with
        | promise ->
            Lwt.on_any promise
              (fun () -> finish operation index (Ok ()))
              (fun exn -> finish operation index (Error exn))
        | exception exn -> finish operation index (Error exn))
      operation.hooks

let operation_report (operation : operation) = operation.report

let bounded (operation : operation) within =
  match Lwt.state operation.report with
  | Lwt.Return report -> Lwt.return report
  | Lwt.Fail exn -> Lwt.fail exn
  | Lwt.Sleep ->
      Lwt.catch
        (fun () ->
          Lwt_unix.with_timeout within (fun () ->
              Lwt.protected operation.report))
        (function
          | Lwt_unix.Timeout -> Lwt.return (snapshot operation)
          | Lwt.Canceled as exn -> Lwt.fail exn
          | exn -> Lwt.fail exn)

let shutdown_report t =
  let action =
    with_lock t (fun () ->
        match t.status with
        | Open when t.hooks = [] ->
            t.status <- Settled (Ok empty_report);
            `Done empty_report
        | Open ->
            let operation_ref = ref None in
            let on_complete outcome =
              with_lock t (fun () ->
                  match (t.status, !operation_ref) with
                  | Closing current, Some operation when current == operation ->
                      t.status <- Settled outcome
                  | Closing _, None -> assert false
                  | Closing _, Some _ -> ()
                  | Open, _ | Settled _, _ -> ())
            in
            let operation, callback =
              make_operation ~on_complete (List.rev t.hooks) (fun hook ->
                  hook.shutdown)
            in
            operation_ref := Some operation;
            t.status <- Closing operation;
            `Start (operation, callback)
        | Closing operation -> `Join operation
        | Settled (Ok report) -> `Done report
        | Settled (Error exn) -> `Failed exn)
  in
  match action with
  | `Start (operation, callback) ->
      start_operation operation callback;
      operation_report operation
  | `Join operation -> operation_report operation
  | `Done report -> Lwt.return report
  | `Failed exn -> Lwt.fail exn

let flush_within t within =
  let action =
    with_lock t (fun () ->
        match t.status with
        | Open when t.hooks = [] -> `Done empty_report
        | Open -> `Start (List.rev t.hooks)
        | Closing operation -> `Join operation
        | Settled (Ok report) -> `Done report
        | Settled (Error exn) -> `Failed exn)
  in
  match action with
  | `Start hooks ->
      let operation, callback = make_operation hooks (fun hook -> hook.flush) in
      start_operation operation callback;
      bounded operation within
  | `Join operation -> bounded operation within
  | `Done report -> Lwt.return report
  | `Failed exn -> Lwt.fail exn

let shutdown_within t within =
  let operation =
    with_lock t (fun () ->
        match t.status with
        | Open | Closing _ -> None
        | Settled outcome -> Some outcome)
  in
  match operation with
  | Some (Ok report) -> Lwt.return report
  | Some (Error exn) -> Lwt.fail exn
  | None -> (
      let report = shutdown_report t in
      let operation =
        with_lock t (fun () ->
            match t.status with
            | Closing operation -> Some operation
            | Open -> None
            | Settled _ -> None)
      in
      match operation with
      | Some operation -> bounded operation within
      | None -> Lwt.map (fun report -> report) report)
