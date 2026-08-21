type message = Message.t
type object_ = Message.object_
type field = Message.field
type open_patch = Message.open_patch

type open_builder = Message.open_builder = {
  untyped : object_;
  field : 'a. string -> 'a Type.t -> 'a -> field;
  object_ : string -> (open_builder -> open_patch) -> field;
  error :
    'error.
    'error Error.t -> ?backtrace:Printexc.raw_backtrace -> 'error -> open_patch;
  seal : object_ -> open_patch;
}

type builder = Message.builder = {
  text : 'a. tag:string -> ('a, Format.formatter, unit, message) format4 -> 'a;
  untyped : object_;
  field : 'a. string -> 'a Type.t -> 'a -> field;
  object_ : string -> (open_builder -> open_patch) -> field;
  seal : object_ -> message;
  value : Value.t -> message;
  error :
    'error.
    'error Error.t -> ?backtrace:Printexc.raw_backtrace -> 'error -> message;
  typed : 'a 'builder. ('a, 'builder) Schema.t -> 'a -> message;
}

type author = builder -> message

let log ~level author = Observer.emit_point ~level author
let debug author = log ~level:Level.Debug author
let info author = log ~level:Level.Info author
let warn author = log ~level:Level.Warn author
let error author = log ~level:Level.Error author
let ( |+ ) = Message.( |+ )

type ('builder, 'patch) t = {
  wide : Engine.wide;
  builder : 'builder;
  materialize : 'patch -> (Engine.contribution, Snapshot.error) result;
}

let create ~name () =
  {
    wide = Observer.create_wide ~name ~origin:Log.Open;
    builder = Message.open_builder;
    materialize =
      (fun patch ->
        Result.map
          (fun body ->
            { Engine.body; has_error = Message.open_patch_has_error patch })
          (Message.open_patch_value patch |> Value.freeze));
  }

let create_typed ~name schema =
  {
    wide =
      Observer.create_wide ~name ~origin:(Log.Declared (Schema.name schema));
    builder = Schema.builder schema;
    materialize =
      (fun patch ->
        Result.map
          (fun body -> { Engine.body; has_error = Schema.has_error patch })
          (Schema.body schema patch));
  }

let set handle author =
  Engine.contribute_wide handle.wide (fun () ->
      handle.materialize (author handle.builder))

let set_level handle = Engine.set_wide_level handle.wide
let emit handle = Engine.emit_wide handle.wide
