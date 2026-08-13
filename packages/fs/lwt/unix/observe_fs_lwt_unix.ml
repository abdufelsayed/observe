module Delivery = Observe_fs_lwt.Make (Platform)

type operation = Platform.operation =
  | Inspect
  | Create_directory
  | Open
  | Write
  | Close

type error =
  | Invalid_path
  | Invalid_capacity of int
  | Filesystem of { operation : operation; path : string; cause : Unix.error }
  | Zero_progress
  | Invalid_write_count of int
  | Unexpected of exn
  | Lifecycle_closed

exception Error of error

let error_of_delivery = function
  | Delivery.Invalid_path -> Invalid_path
  | Delivery.Invalid_capacity capacity -> Invalid_capacity capacity
  | Delivery.Io { operation; path; cause } ->
      Filesystem { operation; path; cause }
  | Delivery.Zero_progress -> Zero_progress
  | Delivery.Invalid_write_count count -> Invalid_write_count count
  | Delivery.Unexpected exn -> Unexpected exn

let pp_error formatter = function
  | Invalid_path -> Format.pp_print_string formatter "invalid filesystem path"
  | Invalid_capacity capacity ->
      Format.fprintf formatter "invalid queue capacity %d" capacity
  | Filesystem { operation; path; cause } ->
      let error : Platform.error = { operation; path; cause } in
      Platform.pp_error formatter error
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
    | Result.Error error -> Lwt.fail (Error (error_of_delivery error)))

let create ~path ?capacity () =
  Lwt.bind (Delivery.create ~path ?capacity ()) (function
    | Result.Error error -> Lwt.return (Result.Error (error_of_delivery error))
    | Result.Ok worker -> (
        match
          Observe_lwt_unix.Lifecycle.register
            ~flush:(lifecycle_hook (fun () -> Delivery.flush worker))
            ~shutdown:(lifecycle_hook (fun () -> Delivery.shutdown worker))
        with
        | Result.Ok () -> Lwt.return (Result.Ok (Delivery.drain worker))
        | Result.Error Observe_lwt_unix.Lifecycle.Closed ->
            Lwt.bind (Delivery.shutdown worker) (fun _ ->
                Lwt.return (Result.Error Lifecycle_closed))))

let create_exn ~path ?capacity () =
  Lwt.bind (create ~path ?capacity ()) (function
    | Result.Ok drain -> Lwt.return drain
    | Result.Error error -> Lwt.fail (Error error))
