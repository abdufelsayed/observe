# Tests

The test tree mirrors the package tree:

- `core` owns public values, formatting, capture, observer state, concurrency,
  PPX behavior, and the installed `observe` surface.
- `lwt` owns configurable Lwt I/O, callback-local propagation, cancellation,
  and the installed `observe-lwt` surface.
- `lwt-unix` owns the ready clock, console, initialization, capture behavior,
  and the installed `observe-lwt-unix` surface.

The package runners are independent:

- `dune build -p observe @install @runtest`
- `dune build -p observe-lwt @install @runtest`
- `dune build -p observe-lwt-unix @install @runtest`

`@correctness` composes those package-owned aliases for local whole-repository
feedback. `@stress` adds randomized and concurrency pressure. Neither replaces
the per-package install/build/test matrix used by CI.
