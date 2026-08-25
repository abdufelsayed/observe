# Tests

The test tree mirrors the package tree:

- `core` owns public values, formatting, capture, observer state, concurrency,
  enrichment and bounded materialization, PPX behavior, and the installed
  `observe` surface.
- `lwt` owns configurable Lwt I/O, callback-local propagation, cancellation,
  and the installed `observe-lwt` surface.
- `lwt-unix` owns the ready clock, console, initialization, capture behavior,
  and the installed `observe-lwt-unix` surface.
- `fs`, `fs/lwt`, and `fs/lwt/unix` mirror the filesystem package family with
  an in-memory state-machine proof, Lwt cancellation proof, real temporary
  directories, installed dependency isolation, and cross-thread delivery.

The package runners are independent:

- `dune build -p observe @install @runtest`
- `dune build -p observe-lwt @install @runtest`
- `dune build -p observe-lwt-unix @install @runtest`
- `dune build -p observe-fs @install @runtest`
- `dune build -p observe-fs-lwt @install @runtest`
- `dune build -p observe-fs-lwt-unix @install @runtest`

`@correctness` composes those package-owned aliases for local whole-repository
feedback. `@stress` adds randomized and concurrency pressure. Neither replaces
the per-package install/build/test matrix used by CI.

## Evidence profiles

Use the repository runner when a local or CI run should leave a durable report:

- `scripts/test.sh quick` runs the deterministic contracts and default property
  counts through `@correctness`.
- `scripts/test.sh stress` reruns correctness plus focused discovery aliases.
  Set `OBSERVE_QCHECK_COUNT`, `OBSERVE_RACE_TRIALS`, and
  `OBSERVE_CONCURRENCY_WORK` to control pressure.

Reports and copied Alcotest failure outputs live under ignored `.logs/` paths.
Every report records the Git revision, OCaml and Dune versions, workload knobs,
and `QCHECK_SEED`. Reproduce a generated failure by rerunning its focused alias
with that seed, then keep the property and add a reduced deterministic
regression when the failure exposes a product bug.

## Ownership and growth

Tests stay with the package and behavior that own them. Shared mechanics belong
in that package's `support` library; generators, models, and replay artifacts
remain beside the surface whose contract they exercise. Add a focused
`discovery` alias only for pressure that is safe without external services.

The core discovery profile currently exercises public value laws, nested
capture routing and conservation, rich formatter projections, temporal stage
boundaries, bounded state, and parameterized publication/capture/diagnostic
concurrency. Fixed unit and compile-surface tests remain in `@correctness`;
install-surface checks remain package-owned gates.

The focused `test/core/enrichment` contract target covers the public enrichment
and limits configuration, point/wide parity, failure isolation, and localized
materialization markers. It is included by `@observe-core-contracts` and
therefore by `@correctness`.

The two user-facing test aliases are `@correctness` and `@stress`. Focused
aliases beneath them express ownership, not additional public profiles.
Performance measurement belongs to the separate maintainer tool under
`bench/`; it is not a test alias or a correctness gate.
