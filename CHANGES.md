# Changes

## Unreleased

- Establish the pure, runtime-neutral structured logging core and sealed public
  API.
- Add process-wide admission-first authoring callbacks with formatted tagged
  text, untyped values, and Repr-backed typed structured values.
- Add validated result-returning configuration with an explicit `_exn` form.
- Define one completed `Observe.IO` contract for effects, dynamic context,
  clock access, and automatic console output.
- Add pure pretty, JSON, and NDJSON formatters, additional drain fan-out,
  bounded diagnostics, and finite deterministic capture.
- Add `observe.ppx` with `[@@deriving observe]`, `[%observe.value ...]`, and
  `[%observe.value.embed ...]`.
- Add `Observe.Make (IO)` for custom integrations and `Observe_lwt.IO` for Lwt
  compositions completed with caller-provided clock and console functions.
- Add the ready `Observe_lwt_unix` initializer with bounded, serialized
  standard-error output, explicit flush and shutdown, and isolated Lwt-scoped
  test capture.
- Add compact UTC tagged text, ordered structured trees, semantic field and
  scalar highlighting, and automatic truecolor, 256-color, 16-color, or plain
  Unix terminal presentation.
- Distribute the portable core, configurable Lwt I/O, and ready Lwt-Unix
  composition as separate opam packages while keeping `observe.ppx` with the
  core package.
- Establish package-owned correctness and discovery gates with model-based
  capture properties, rich formatter projections, parameterized concurrency
  pressure, temporal and bounded-space laws, and reproducible stress reports.
- Add a separate Bechamel maintainer tool for component, controlled-core, and
  real Lwt-Unix measurements with structured JSON reports.
- Add dependency-isolated `observe-fs`, `observe-fs-lwt`, and
  `observe-fs-lwt-unix` packages with bounded daily NDJSON delivery, typed
  setup errors, synchronous ownership transfer, and the shared ready
  Lwt-Unix flush and shutdown lifecycle.
