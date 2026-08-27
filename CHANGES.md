# Changes

## Unreleased

## 0.1.0-alpha.1 - 2026-08-28

This is the first alpha release. Public APIs may change before 1.0.

### Logging experience

- Establish the pure, runtime-neutral structured logging core and sealed public
  API.
- Add process-wide admission-first authoring callbacks with formatted tagged
  text, untyped values, and Repr-backed typed structured values.
- Add validated result-returning configuration with an explicit `_exn` form.
- Define one completed `Observe.IO` contract for effects, dynamic context,
  clock access, and automatic console output.
- Add pure pretty, JSON, and NDJSON formatters, additional drain fan-out,
  bounded diagnostics, and finite deterministic capture.
- Add `observe.ppx` with admission-preserving logging extensions,
  `[@@deriving observe]`, `[%observe.value ...]`, and
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
- Add open and schema-locked wide events with incremental lazy contributions,
  explicit levels and annotations, at-most-once completion, searchable
  occurrence identity, monotonic duration, and causal child references without
  parent-payload copying. The ready Lwt-Unix package generates UUID v4
  identities by default.
- Add scoped Lwt operations and child forks that preserve callback results,
  exceptions, raw backtraces, and cancellation while keeping manual wide-event
  lifecycles available.
- Add explicit reusable error interpretation without ambient exception hooks or
  a universal application error type.

### Production policy and safety

- Add flat configuration for reusable enrichers, finite materialization limits,
  caller-defined redaction, per-level sampling, completion-aware retention, and
  additional drains.
- Bound every published typed or open point and wide value by depth, object
  fields, collection length, string and byte length, productive nodes, and
  deterministic total-size accounting.
- Preserve safe prefixes and siblings with package-owned truncation markers and
  withhold observations that cannot reach a safe canonical boundary.
- Add exact-path and value redaction with removal, fixed replacement, finite
  masks, schema validation, fail-closed traversal, and stricter per-drain
  projections. Observe intentionally ships no domain sensitivity presets.
- Add deterministic and correlation-stable sampling, completion-aware rescue,
  safe drain routing, and final retention before fan-out.

### Delivery and operation

- Keep `Observe.Drain.t` as the prompt portable ownership-transfer seam while
  ready console and filesystem facilities own independent finite queues,
  workers, rejection, loss, and lifecycle behavior.
- Add truthful finite flush and idempotent shutdown boundaries with advanced
  reports for rejection, accepted-work loss, destination failure, timeout, and
  cancellation. Acceptance and local completion make no durability claim.
- Add a complete production example, installed human guide, package chooser,
  and compiled point, wide-event, policy, output, lifecycle, and testing paths.
- Expand the benchmark matrix across 140 component, core, runtime, and
  filesystem scenarios with isolated repetitions, allocation, retained-size,
  and encoded-size evidence.
