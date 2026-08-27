module Writer = Observe_fs_lwt.Make (Io)

type operation = Io.operation =
  | Inspect
  | Create_directory
  | Open
  | Write
  | Close

type error =
  | Invalid_directory
  | Invalid_capacity of int
  | Filesystem of { operation : operation; path : string; cause : Unix.error }
  | Zero_progress
  | Invalid_write_count of int
  | Unexpected of exn
  | Lifecycle_closed

exception Error of error

let error_of_writer = function
  | Writer.Invalid_directory -> Invalid_directory
  | Writer.Invalid_capacity capacity -> Invalid_capacity capacity
  | Writer.Io { operation; path; cause } ->
      Filesystem { operation; path; cause }
  | Writer.Zero_progress -> Zero_progress
  | Writer.Invalid_write_count count -> Invalid_write_count count
  | Writer.Unexpected exn -> Unexpected exn

let pp_error formatter = function
  | Invalid_directory ->
      Format.pp_print_string formatter "invalid filesystem directory"
  | Invalid_capacity capacity ->
      Format.fprintf formatter "invalid queue capacity %d" capacity
  | Filesystem { operation; path; cause } ->
      let error : Io.error = { operation; path; cause } in
      Io.pp_error formatter error
  | Zero_progress ->
      Format.pp_print_string formatter "filesystem write made no progress"
  | Invalid_write_count count ->
      Format.fprintf formatter "filesystem returned invalid write count %d"
        count
  | Unexpected exn ->
      Format.fprintf formatter "unexpected filesystem exception: %s"
        (Printexc.to_string exn)
  | Lifecycle_closed ->
      Format.pp_print_string formatter "Observe Lwt-Unix lifecycle is closed"

let lifecycle_hook callback () =
  Lwt.bind (callback ()) (function
    | Result.Ok () -> Lwt.return_unit
    | Result.Error error -> Lwt.fail (Error (error_of_writer error)))

let cleanup writer =
  Lwt.catch
    (fun () -> Lwt.map ignore (Lwt.no_cancel (Writer.shutdown writer)))
    (function
      | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> Lwt.fail exn
      | _ -> Lwt.return_unit)

let next_output = Atomic.make 0

let output_label () =
  let number = Atomic.fetch_and_add next_output 1 + 1 in
  Printf.sprintf "filesystem-%d" number

let create ~dir ?capacity () =
  Lwt.bind (Writer.create ~dir ?capacity ()) (function
    | Result.Error error -> Lwt.return (Result.Error (error_of_writer error))
    | Result.Ok writer -> (
        let facts () =
          match Writer.delivery_facts writer with
          | Writer.No_problem ->
              Observe_lwt_unix.Lifecycle.Integration.No_problem
          | Writer.Rejected -> Observe_lwt_unix.Lifecycle.Integration.Rejected
          | Writer.Delivery_lost ->
              Observe_lwt_unix.Lifecycle.Integration.Delivery_lost
          | Writer.Rejected_and_lost ->
              Observe_lwt_unix.Lifecycle.Integration.Rejected_and_lost
        in
        let label = output_label () in
        match
          Observe_lwt_unix.Lifecycle.Integration.register ~label ~facts
            ~flush:(lifecycle_hook (fun () -> Writer.flush writer))
            ~shutdown:(lifecycle_hook (fun () -> Writer.shutdown writer))
        with
        | Result.Ok () -> Lwt.return (Result.Ok (Writer.drain writer))
        | Result.Error Observe_lwt_unix.Lifecycle.Integration.Closed ->
            Lwt.bind (cleanup writer) (fun () ->
                Lwt.return (Result.Error Lifecycle_closed))
        | Result.Error Observe_lwt_unix.Lifecycle.Integration.Invalid_label ->
            Lwt.bind (cleanup writer) (fun () ->
                Lwt.return
                  (Result.Error
                     (Unexpected
                        (Invalid_argument
                           "Observe_fs_lwt_unix: invalid internal output label"))))
        | exception raised ->
            let backtrace = Printexc.get_raw_backtrace () in
            Lwt.bind (cleanup writer) (fun () ->
                Printexc.raise_with_backtrace raised backtrace)))

let create_exn ~dir ?capacity () =
  Lwt.bind (create ~dir ?capacity ()) (function
    | Result.Ok drain -> Lwt.return drain
    | Result.Error error -> Lwt.fail (Error error))
