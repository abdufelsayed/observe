module Type = Type
module Schema_internal = Schema

module Schema = struct
  include Schema_internal

  let field = field_patch
  let nested = nested_patch
end

module Error = Error
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

module Ppx_runtime = struct
  type 'a type_description = 'a Type.t
  type logs_message = Logs.message
  type logs_untyped_patch = Logs.untyped_patch

  module Type = struct
    type 'a description = 'a type_description

    include Type.Ppx_runtime
  end

  module Schema = struct
    type fragment = Schema_internal.fragment
    type patch_field = Schema_internal.field

    let fragment = Schema_internal.fragment

    let error_fragment interpretation ?backtrace error =
      Schema_internal.fragment_of_value
        (Error.value interpretation ?backtrace error)

    let patch_fragment = Schema_internal.patch_fragment
    let patch_field = Schema_internal.field
    let record_patch = Schema_internal.make_patch
    let record_patch_fields = Schema_internal.make_patch_fields
    let identified_record_patch = Schema_internal.make_identified_patch

    let identified_record_patch_fields =
      Schema_internal.make_identified_patch_fields

    let identified_error_patch = Schema_internal.make_identified_error_patch
    let combine_identified_patches = Schema_internal.combine_identified_patches
    let record_schema = Schema_internal.record
    let schema_builder = Schema_internal.builder
  end

  module Logs = struct
    type message = logs_message
    type untyped_patch = logs_untyped_patch

    let untyped_value_patch = Message.untyped_patch_of_value
    let untyped_message = Message.untyped_message_of_value
    let is_reserved_field = Log_envelope.is_reserved_field
  end
end

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
