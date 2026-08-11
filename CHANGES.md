# Changes

## Unreleased

- Establish the pure, runtime-neutral structured logging core and sealed public
  API.
- Add process-wide tagged text, deferred text, deferred free-form values, and
  Repr-backed typed structured authoring.
- Add validated result-returning configuration with an explicit `_exn` form.
- Separate runtime dynamic context from platform clock and automatic terminal
  output.
- Add pure readable, JSON, and JSON Lines formatters, additional drain fan-out,
  bounded diagnostics, and finite deterministic capture.
- Add `observe.ppx` with `[@@deriving observe]`, `[%observe.value ...]`, and
  `[%observe.value.embed ...]`.
- Add `Observe_lwt.Runtime` and `Observe_unix.Platform` as independently
  composable mechanism sublibraries.
- Add the ready `Observe_lwt_unix` initializer with automatic standard-error
  output and isolated Lwt-scoped test capture.
