module Platform = struct
  type clock_error = Observe_engine.clock_error = Unavailable

  type terminal_acceptance = Observe_engine.terminal_acceptance =
    | Accepted
    | Rejected

  module type S = sig
    type t

    val now : t -> (Observe_instant.t, clock_error) result
    val write_terminal : t -> string -> terminal_acceptance
  end
end

module type S = sig
  type +'a t
  type context
  type 'a key

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val create_key : unit -> 'a key
  val get : context -> 'a key -> 'a option
  val with_binding : context -> 'a key -> 'a -> (unit -> 'b t) -> 'b t
  val protect : context -> finally:(unit -> unit) -> (unit -> 'a t) -> 'a t
  val is_control_exception : context -> exn -> bool
end

type init_error = Already_initialized | Runtime_already_registered
type capture_error = Runtime_already_registered | Invalid_capacity of int

exception Init_error of init_error

type runtime = {
  resolve_capture : unit -> capture_scope option;
  is_control_exception : exn -> bool;
}

and capture_scope = {
  engine : Observe_engine.t;
  capture : Observe_capture.t;
  closed : bool Atomic.t;
}

type route =
  | Vacant
  | Runtime_registered of runtime
  | Production of runtime * Observe_engine.t

let route = Atomic.make Vacant

let rec claim_runtime runtime : (unit, capture_error) result =
  match Atomic.get route with
  | Vacant as before ->
      if Atomic.compare_and_set route before (Runtime_registered runtime) then
        Ok ()
      else claim_runtime runtime
  | (Runtime_registered installed | Production (installed, _))
    when installed == runtime ->
      Ok ()
  | Runtime_registered _ | Production _ -> Error Runtime_already_registered

let rec publish runtime engine : (unit, init_error) result =
  match Atomic.get route with
  | Vacant as before ->
      if Atomic.compare_and_set route before (Production (runtime, engine)) then (
        Observe_engine.after_production_install engine;
        Ok ())
      else publish runtime engine
  | Runtime_registered installed as before when installed == runtime ->
      if Atomic.compare_and_set route before (Production (runtime, engine)) then (
        Observe_engine.after_production_install engine;
        Ok ())
      else publish runtime engine
  | Production (installed, _) when installed == runtime ->
      Error Already_initialized
  | Runtime_registered _ | Production _ -> Error Runtime_already_registered

type resolution = Engine of Observe_engine.t | Missing | Withhold

let resolve runtime fallback =
  match
    Observe_engine.contain ~is_control_exception:runtime.is_control_exception
      runtime.resolve_capture
  with
  | Observe_engine.Raised ->
      Observe_diagnostics.record Observe_diagnostics.Scope_raised;
      Withhold
  | Observe_engine.Returned None -> fallback
  | Observe_engine.Returned (Some scope) ->
      if Atomic.get scope.closed then (
        Observe_capture.record scope.capture Observe_diagnostics.Capture_closed;
        Withhold)
      else Engine scope.engine

let active_engine () =
  match Atomic.get route with
  | Vacant -> Missing
  | Runtime_registered runtime -> resolve runtime Missing
  | Production (runtime, engine) -> resolve runtime (Engine engine)

let emit ~level message =
  match active_engine () with
  | Engine engine -> Observe_engine.emit engine level message
  | Withhold -> ()
  | Missing -> Observe_diagnostics.record Observe_diagnostics.Not_initialized

module Make (Runtime : S) (Platform : Platform.S) = struct
  type +'a io = 'a Runtime.t

  let capture_key : capture_scope Runtime.key = Runtime.create_key ()

  type t = {
    runtime_context : Runtime.context;
    platform : Platform.t;
    runtime : runtime;
  }

  let create ~runtime_context ~platform =
    let runtime =
      {
        resolve_capture = (fun () -> Runtime.get runtime_context capture_key);
        is_control_exception = Runtime.is_control_exception runtime_context;
      }
    in
    { runtime_context; platform; runtime }

  let clock t () = Platform.now t.platform
  let terminal t output = Platform.write_terminal t.platform output

  let engine t config output =
    let is_control_exception = t.runtime.is_control_exception in
    match output with
    | `Production ->
        Observe_engine.create_production config ~clock:(clock t)
          ~terminal:(terminal t) ~is_control_exception
    | `Capture capture ->
        Observe_engine.create_capture config ~clock:(clock t)
          ~is_control_exception capture

  let init t config = publish t.runtime (engine t config `Production)

  let init_exn t config =
    match init t config with
    | Ok () -> ()
    | Error error -> raise (Init_error error)

  let close_scope scope =
    if Atomic.compare_and_set scope.closed false true then
      Observe_capture.close scope.capture

  let with_capture t config ?capacity callback =
    let capacity =
      match capacity with
      | Some capacity -> capacity
      | None -> Observe_capture.default_capacity
    in
    if capacity <= 0 then Runtime.return (Error (Invalid_capacity capacity))
    else
      match claim_runtime t.runtime with
      | Error error -> Runtime.return (Error error)
      | Ok () ->
          let capture = Observe_capture.create ~capacity in
          let scope =
            {
              engine = engine t config (`Capture capture);
              capture;
              closed = Atomic.make false;
            }
          in
          Runtime.protect t.runtime_context
            ~finally:(fun () -> close_scope scope)
            (fun () ->
              Runtime.with_binding t.runtime_context capture_key scope
                (fun () ->
                  Runtime.bind (callback capture) (fun result ->
                      Runtime.return (Ok result))))
end
