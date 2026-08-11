# Observe tests

The suite is organized by observable boundary:

- `public` proves pure public values and configuration.
- `capture` proves scoped logging and formatter behavior.
- `runtime` runs each irreversible process-state scenario in a fresh process.
- `lwt` proves real callback-local propagation, cancellation, and the ready
  Lwt-Unix composition.
- `unix` proves the OS clock and exact standard-error write mechanism.
- `concurrency` targets atomic publication, capture, and diagnostics.
- `interfaces` checks source consumers and the installed private-module surface.
- `ppx` proves generated behavior and stable package-owned diagnostics.

Run `dune build @correctness` for the fast no-network gate. Run
`OBSERVE_QCHECK_COUNT=2000 OBSERVE_RACE_TRIALS=100
OBSERVE_CONCURRENCY_WORK=64 dune build @stress` for higher randomized and
concurrency pressure.
