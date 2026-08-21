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
  let named_record_patch = Schema.make_named_patch
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

module Make = Observer.Make
