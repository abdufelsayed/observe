module Type = Type
module Schema = Schema
module Error = Error

module Generated_runtime = struct
  type 'a description = 'a Type.t

  include Type.Generated_runtime

  type fragment = Schema.fragment
  type patch_field = Schema.field
  type untyped_patch = Message.untyped_patch

  let fragment = Schema.fragment

  let error_fragment interpretation ?backtrace error =
    Schema.fragment_of_result (Error.freeze interpretation ?backtrace error)

  let patch_fragment = Schema.patch_fragment
  let patch_field = Schema.field
  let record_patch = Schema.make_patch
  let record_patch_fields = Schema.make_patch_fields
  let identified_record_patch = Schema.make_identified_patch
  let identified_record_patch_fields = Schema.make_identified_patch_fields
  let identified_error_patch = Schema.make_identified_error_patch
  let combine_identified_patches = Schema.combine_identified_patches
  let record_schema = Schema.record
  let schema_builder = Schema.builder
  let untyped_value_patch = Message.untyped_patch_of_value
end

module Level = Level
module Timestamp = Timestamp
module Value = Value
module Log = Log
module Diagnostics = Diagnostics
module Drain = Drain
module Formatter = Formatter
module Capture = Capture
module Config = Config
module Logs = Logs
module IO = Io

type init_error = Observer.init_error =
  | Already_initialized
  | IO_already_registered

type capture_error = Observer.capture_error =
  | IO_already_registered
  | Invalid_capacity of int

exception Init_error = Observer.Init_error

module Make (IO : Io.S) = struct
  module Runtime = Observer.Make (IO)

  type +'a io = 'a IO.t
  type t = { runtime : Runtime.t; state : IO.state }

  let create state = { runtime = Runtime.create state; state }
  let init t = Runtime.init t.runtime
  let init_exn t = Runtime.init_exn t.runtime
  let with_capture t ~config = Runtime.with_capture t.runtime ~config

  let is_control_exception t raised =
    match raised with
    | Out_of_memory | Stack_overflow | Sys.Break -> true
    | _ -> (
        match IO.is_control_exception t.state raised with
        | control -> control
        | exception _ -> true)

  let run_operation t wide ~error callback =
    Runtime.with_operation t.runtime (Logs.engine_current wide) (fun () ->
        IO.bind (IO.observe callback) (function
          | Io.Returned value ->
              Logs.emit wide;
              IO.return value
          | Io.Raised (raised, backtrace) ->
              let () =
                if is_control_exception t raised then Logs.emit wide
                else
                  let contributed =
                    match
                      Logs.contribute_error wide error ~backtrace raised
                    with
                    | contributed -> contributed
                    | exception _ -> false
                  in
                  if contributed then Logs.emit wide
              in
              IO.repropagate raised backtrace))

  let with_operation t ~name ?using ?(error = Error.exn) callback =
    match using with
    | None -> run_operation t (Logs.create ~name ()) ~error callback
    | Some using ->
        run_operation t (Logs.create_typed ~name ~using ()) ~error callback

  let fork t ~parent ~name ?using ?(error = Error.exn) callback =
    match using with
    | None -> run_operation t (Logs.create ~parent ~name ()) ~error callback
    | Some using ->
        run_operation t
          (Logs.create_typed ~parent ~name ~using ())
          ~error callback
end
