(** Test-only characterization of the generated untyped logging bridge.

    Ordinary fixtures should prefer the public [Observe.Logs] builders. Tests
    that need an arbitrary [Observe.Value.t] use this helper so dependence on
    the PPX/runtime ABI stays explicit and centralized. *)

let of_value value (_ : Observe.Logs.builder) =
  Observe.Ppx_runtime.Logs.untyped_message value

let untyped make builder = of_value (make ()) builder
