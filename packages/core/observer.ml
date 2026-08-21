type init_error = Already_initialized | IO_already_registered
type capture_error = IO_already_registered | Invalid_capacity of int

exception Init_error of init_error

type io_registration = {
  resolve_capture : unit -> capture_scope option;
  is_control_exception : exn -> bool;
}

and capture_scope = {
  engine : Engine.t;
  capture : Capture.t;
  closed : bool Atomic.t;
}

type route =
  | Vacant
  | Io_registered of io_registration
  | Outputs of io_registration * Engine.t

let route = Atomic.make Vacant

let rec claim_io io : (unit, capture_error) result =
  match Atomic.get route with
  | Vacant as before ->
      if Atomic.compare_and_set route before (Io_registered io) then Ok ()
      else claim_io io
  | (Io_registered installed | Outputs (installed, _)) when installed == io ->
      Ok ()
  | Io_registered _ | Outputs _ -> Error IO_already_registered

let rec publish io engine : (unit, init_error) result =
  match Atomic.get route with
  | Vacant as before ->
      if Atomic.compare_and_set route before (Outputs (io, engine)) then (
        Engine.after_install engine;
        Ok ())
      else publish io engine
  | Io_registered installed as before when installed == io ->
      if Atomic.compare_and_set route before (Outputs (io, engine)) then (
        Engine.after_install engine;
        Ok ())
      else publish io engine
  | Outputs (installed, _) when installed == io -> Error Already_initialized
  | Io_registered _ | Outputs _ -> Error IO_already_registered

type resolution = Engine of Engine.t | Missing | Withhold

let resolve io fallback =
  match
    Engine.contain ~is_control_exception:io.is_control_exception
      io.resolve_capture
  with
  | Engine.Raised ->
      Diagnostics.record Diagnostics.Capture_lookup_raised;
      Withhold
  | Engine.Returned None -> fallback
  | Engine.Returned (Some scope) ->
      if Atomic.get scope.closed then (
        Capture.record scope.capture Diagnostics.Capture_closed;
        Withhold)
      else Engine scope.engine

let active_engine () =
  match Atomic.get route with
  | Vacant -> Missing
  | Io_registered io -> resolve io Missing
  | Outputs (io, engine) -> resolve io (Engine engine)

let emit_point ~level author =
  match active_engine () with
  | Engine engine -> Engine.emit_point engine level author
  | Withhold -> ()
  | Missing -> Diagnostics.record Diagnostics.Not_initialized

let create_wide ~name ~origin =
  match active_engine () with
  | Engine engine -> Engine.create_wide engine ~name ~origin
  | Withhold | Missing -> Engine.inert_wide ()

module Make (IO : Io.S) = struct
  type +'a io = 'a IO.t

  let capture_key : capture_scope IO.key = IO.create_key ()

  type t = { state : IO.state; io : io_registration }

  let create state =
    let io =
      {
        resolve_capture = (fun () -> IO.get state capture_key);
        is_control_exception = IO.is_control_exception state;
      }
    in
    { state; io }

  let clock t () = IO.Clock.now t.state
  let monotonic_now t () = IO.Clock.monotonic_now t.state
  let next_id t () = IO.Identity.next t.state
  let offer_console t output = IO.Console.offer t.state output

  let engine t config output =
    let is_control_exception = t.io.is_control_exception in
    match output with
    | `Outputs ->
        Engine.create_outputs config ~console_style:(IO.Console.style t.state)
          ~clock:(clock t) ~monotonic_now:(monotonic_now t) ~next_id:(next_id t)
          ~console:(offer_console t) ~is_control_exception
    | `Capture capture ->
        Engine.create_capture config ~clock:(clock t)
          ~monotonic_now:(monotonic_now t) ~next_id:(next_id t)
          ~is_control_exception capture

  let init t config = publish t.io (engine t config `Outputs)

  let init_exn t config =
    match init t config with
    | Ok () -> ()
    | Error error -> raise (Init_error error)

  let close_scope scope =
    if Atomic.compare_and_set scope.closed false true then
      Capture.close scope.capture

  let with_capture t config ?capacity callback =
    let capacity =
      match capacity with
      | Some capacity -> capacity
      | None -> Capture.default_capacity
    in
    if capacity <= 0 then IO.return (Error (Invalid_capacity capacity))
    else
      match claim_io t.io with
      | Error error -> IO.return (Error error)
      | Ok () ->
          let capture = Capture.create ~capacity in
          let scope =
            {
              engine = engine t config (`Capture capture);
              capture;
              closed = Atomic.make false;
            }
          in
          IO.protect t.state
            ~finally:(fun () -> close_scope scope)
            (fun () ->
              IO.with_binding t.state capture_key scope (fun () ->
                  IO.bind (callback capture) (fun result ->
                      IO.return (Ok result))))
end
