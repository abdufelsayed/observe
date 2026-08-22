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

type ('builder, 'patch) t = {
  wide : Engine.wide;
  builder : 'builder;
  materialize : 'patch -> Engine.contribution;
  error_materialize :
    'error.
    'error Error.t ->
    ?backtrace:Printexc.raw_backtrace ->
    'error ->
    Engine.contribution;
}

let engine_wide handle = handle.wide
let operation_id handle = Engine.wide_id handle.wide

let log ?operation ~level author =
  let correlation_id = Option.bind operation operation_id in
  Observer.emit_point ?correlation_id ~level author

let debug ?operation author = log ?operation ~level:Level.Debug author
let info ?operation author = log ?operation ~level:Level.Info author
let warn ?operation author = log ?operation ~level:Level.Warn author
let error ?operation author = log ?operation ~level:Level.Error author
let ( |+ ) = Message.( |+ )

let materialize_error interpretation ?backtrace error =
  match Error.freeze interpretation ?backtrace error with
  | Ok body -> Engine.Contribution (body, true)
  | Error error -> Engine.Invalid_contribution error

let create ?parent ~name () =
  {
    wide =
      Observer.create_wide
        ?parent:(Option.map engine_wide parent)
        ~name ~origin:Log.Open ();
    builder = Message.open_builder;
    materialize =
      (fun patch ->
        match Message.open_patch_fragment patch with
        | Ok body ->
            Engine.Contribution (body, Message.open_patch_has_error patch)
        | Error error -> Engine.Invalid_contribution error);
    error_materialize = materialize_error;
  }

let create_typed ?parent ~name schema =
  {
    wide =
      Observer.create_wide
        ?parent:(Option.map engine_wide parent)
        ~name
        ~origin:(Log.Declared (Schema.name schema))
        ();
    builder = Schema.builder schema;
    materialize =
      (fun patch ->
        match Schema.body schema patch with
        | Ok body -> Engine.Contribution (body, Schema.has_error patch)
        | Error error -> Engine.Invalid_contribution error);
    error_materialize = materialize_error;
  }

let set handle author =
  Engine.contribute_wide handle.wide (fun () ->
      handle.materialize (author handle.builder))

let contribute_error handle interpretation ?backtrace error =
  Engine.contribute_wide handle.wide (fun () ->
      handle.error_materialize interpretation ?backtrace error)

let set_level handle = Engine.set_wide_level handle.wide
let emit handle = Engine.emit_wide handle.wide

module Terminal = struct
  type ('builder, 'patch) log = ('builder, 'patch) t

  type ('builder, 'patch) t = {
    error : exn Error.t;
    log : ('builder, 'patch) log;
    claimed : bool Atomic.t;
  }

  let create ~error log = { error; log; claimed = Atomic.make false }

  let claim terminal complete =
    if Atomic.compare_and_set terminal.claimed false true then complete ()

  let contribute_final terminal = function
    | None -> ()
    | Some author -> set terminal.log author

  let complete terminal ?set:final () =
    claim terminal (fun () ->
        contribute_final terminal final;
        emit terminal.log)

  let fail terminal ?set:final ?backtrace raised =
    claim terminal (fun () ->
        contribute_final terminal final;
        contribute_error terminal.log terminal.error ?backtrace raised;
        emit terminal.log)

  let cancel terminal ?set () = complete terminal ?set ()
end
