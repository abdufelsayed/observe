type message = Message.t
type object_ = Message.object_
type field = Message.field
type untyped_patch = Message.untyped_patch

type untyped_builder = Message.untyped_builder = {
  untyped : object_;
  field : 'a. string -> 'a Type.t -> 'a -> field;
  object_ : string -> (untyped_builder -> untyped_patch) -> field;
  error :
    'error.
    using:'error Error.t ->
    ?backtrace:Printexc.raw_backtrace ->
    'error ->
    untyped_patch;
  seal : object_ -> untyped_patch;
}

type builder = Message.builder = {
  text : 'a. tag:string -> ('a, Format.formatter, unit, message) format4 -> 'a;
  untyped : object_;
  field : 'a. string -> 'a Type.t -> 'a -> field;
  object_ : string -> (untyped_builder -> untyped_patch) -> field;
  seal : object_ -> message;
  value : Value.t -> message;
  error :
    'error.
    using:'error Error.t ->
    ?backtrace:Printexc.raw_backtrace ->
    'error ->
    message;
  typed : 'a 'builder. using:('a, 'builder) Schema.t -> 'a -> message;
}

type author = builder -> message

type ('builder, 'patch) t = {
  wide : Engine.wide;
  mode : mode;
  builder : 'builder;
  materialize : 'patch -> Engine.contribution;
  error_materialize :
    'error.
    'error Error.t ->
    ?backtrace:Printexc.raw_backtrace ->
    'error ->
    Engine.contribution;
}

and mode = Open | Typed of Schema.identity

type current_error =
  | Not_bound
  | Expected_open
  | Expected_typed
  | Schema_mismatch

exception Current_error of current_error

let engine_wide handle = handle.wide

let engine_current handle =
  match handle.mode with
  | Open -> Engine.Open handle.wide
  | Typed schema_id -> Engine.Typed (handle.wide, schema_id)

let operation_reference handle = Engine.wide_reference handle.wide

let log ?operation ~level author =
  let correlation = Option.bind operation operation_reference in
  Observer.emit_point ?correlation ~level author

let debug ?operation author = log ?operation ~level:Level.Debug author
let info ?operation author = log ?operation ~level:Level.Info author
let warn ?operation author = log ?operation ~level:Level.Warn author
let error ?operation author = log ?operation ~level:Level.Error author
let ( |+ ) = Message.( |+ )

let materialize_error interpretation ?backtrace error =
  match Error.freeze interpretation ?backtrace error with
  | Ok body -> Engine.Contribution (body, true)
  | Error error -> Engine.Invalid_contribution error

let open_handle wide =
  {
    wide;
    mode = Open;
    builder = Message.untyped_builder;
    materialize =
      (fun patch ->
        match Message.untyped_patch_fragment patch with
        | Ok body ->
            Engine.Contribution (body, Message.untyped_patch_has_error patch)
        | Error error -> Engine.Invalid_contribution error);
    error_materialize = materialize_error;
  }

let create ?parent ~name () =
  Observer.create_wide
    ?parent:(Option.map engine_wide parent)
    ~name ~origin:Log.Open ()
  |> open_handle

let typed_handle wide schema =
  {
    wide;
    mode = Typed (Schema.identity schema);
    builder = Schema.builder schema;
    materialize =
      (fun patch ->
        match Schema.body schema patch with
        | Ok body -> Engine.Contribution (body, Schema.has_error patch)
        | Error error -> Engine.Invalid_contribution error);
    error_materialize = materialize_error;
  }

let create_typed ?parent ~name ~using () =
  Observer.create_wide
    ?parent:(Option.map engine_wide parent)
    ~name
    ~origin:(Log.Declared (Schema.name using))
    ()
  |> fun wide -> typed_handle wide using

let current () =
  match Observer.current_operation () with
  | None -> raise (Current_error Not_bound)
  | Some (Engine.Open wide) -> open_handle wide
  | Some (Engine.Typed _) -> raise (Current_error Expected_open)

let current_typed ~using =
  match Observer.current_operation () with
  | None -> raise (Current_error Not_bound)
  | Some (Engine.Open _) -> raise (Current_error Expected_typed)
  | Some (Engine.Typed (wide, identity)) ->
      if Schema.same_identity identity (Schema.identity using) then
        typed_handle wide using
      else raise (Current_error Schema_mismatch)

let set handle author =
  ignore
    (Engine.contribute_wide handle.wide (fun () ->
         handle.materialize (author handle.builder)))

let contribute_error handle interpretation ?backtrace error =
  Engine.contribute_wide handle.wide (fun () ->
      handle.error_materialize interpretation ?backtrace error)

let set_level handle ~level = Engine.set_wide_level handle.wide level

let annotate handle ~level author =
  ignore (Engine.annotate_wide handle.wide level author)

let emit handle = Engine.emit_wide handle.wide
