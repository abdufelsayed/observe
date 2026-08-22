module Type = Type
module Schema = Schema
module Error = Error

module Generated_runtime = struct
  type 'a description = 'a Type.t

  include Type.Generated_runtime

  type fragment = Schema.fragment
  type patch_field = Schema.field
  type open_patch = Message.open_patch

  let fragment = Schema.fragment

  let error_fragment interpretation ?backtrace error =
    Schema.fragment_of_result (Error.freeze interpretation ?backtrace error)

  let patch_fragment = Schema.patch_fragment
  let patch_field = Schema.field
  let record_patch = Schema.make_patch
  let record_patch_fields = Schema.make_patch_fields
  let named_record_patch = Schema.make_named_patch
  let named_record_patch_fields = Schema.make_named_patch_fields
  let named_error_patch = Schema.make_named_error_patch
  let combine_named_patches = Schema.combine_named_patches
  let record_schema = Schema.record
  let schema_builder = Schema.builder
  let open_value_patch = Message.open_patch_of_value
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
  let with_capture t = Runtime.with_capture t.runtime

  let with_wide t wide callback =
    Runtime.with_wide t.runtime (Logs.engine_wide wide) callback

  let is_control_exception t raised =
    match raised with
    | Out_of_memory | Stack_overflow | Sys.Break -> true
    | _ -> (
        match IO.is_control_exception t.state raised with
        | control -> control
        | exception _ -> true)

  let manage t wide ~error callback =
    with_wide t wide (fun () ->
        IO.bind (IO.observe callback) (function
          | Io.Returned value ->
              Logs.emit wide;
              IO.return value
          | Io.Raised (raised, backtrace) ->
              if not (is_control_exception t raised) then
                Logs.contribute_error wide error ~backtrace raised;
              Logs.emit wide;
              IO.repropagate raised backtrace))

  let fork t ~parent ~name ~error callback =
    let child = Logs.create ~parent ~name () in
    manage t child ~error (fun () -> callback child)

  let fork_typed t ~parent ~name schema ~error callback =
    let child = Logs.create_typed ~parent ~name schema in
    manage t child ~error (fun () -> callback child)
end
