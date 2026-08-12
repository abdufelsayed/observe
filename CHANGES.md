# Changes

## Unreleased

- Establish the pure, runtime-neutral structured logging core and sealed public
  API.
- Add process-wide tagged text, deferred text, deferred free-form values, and
  Repr-backed typed structured authoring.
- Add validated result-returning configuration with an explicit `_exn` form.
- Define one completed `Observe.IO` contract for effects, dynamic context,
  clock access, and automatic console output.
- Add pure readable, JSON, and JSON Lines formatters, additional drain fan-out,
  bounded diagnostics, and finite deterministic capture.
- Add `observe.ppx` with `[@@deriving observe]`, `[%observe.value ...]`, and
  `[%observe.value.embed ...]`.
- Add `Observe.Make (IO)` for custom integrations and `Observe_lwt.IO` for Lwt
  compositions completed with caller-provided clock and console functions.
- Add the ready `Observe_lwt_unix` initializer with automatic standard-error
  output and isolated Lwt-scoped test capture.
- Add compact UTC tagged text, ordered structured trees, semantic field and
  scalar highlighting, and automatic truecolor, 256-color, 16-color, or plain
  Unix terminal presentation.
- Distribute the portable core, configurable Lwt I/O, and ready Lwt-Unix
  composition as separate opam packages while keeping `observe.ppx` with the
  core package.
