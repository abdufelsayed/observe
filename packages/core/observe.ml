module Type = Type
module Level = Level
module Instant = Instant
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
