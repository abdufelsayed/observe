# Changes

## Unreleased

- Establish the pure, runtime-neutral structured logging core and sealed public
  API.
- Add process-wide tagged text, deferred text, deferred free-form values, and
  Repr-backed typed structured authoring.
- Add validated result-returning configuration with an explicit `_exn` form.
- Separate runtime dynamic context from platform clock and automatic console
  output.
- Add pure readable, JSON, and JSON Lines formatters, additional drain fan-out,
  bounded diagnostics, and finite deterministic capture.
- Add `observe.ppx` with `[@@deriving observe]`, `[%observe.value ...]`, and
  `[%observe.value.embed ...]`.
- Add `Observe_lwt.Runtime` and `Observe_unix.Platform` as independently
  composable mechanism packages.
- Add the ready `Observe_lwt_unix` initializer with automatic standard-error
  output and isolated Lwt-scoped test capture.
- Add compact UTC tagged text, ordered structured trees, semantic field and
  scalar highlighting, and automatic truecolor, 256-color, 16-color, or plain
  Unix terminal presentation.
- Distribute the portable core, Lwt runtime, Unix platform, and ready Lwt-Unix
  composition as separate opam packages while keeping `observe.ppx` with the
  core package.
