# Observe benchmarks

The benchmark executable is a maintainer tool. It does not belong to the test
suite and its measurements do not pass or fail correctness gates.

Run every scenario:

```sh
opam exec -- dune exec bench/observe_bench.exe
```

Select one suite or compare with an earlier report:

```sh
opam exec -- dune exec bench/observe_bench.exe -- --suite core
opam exec -- dune exec bench/observe_bench.exe -- \
  --compare .logs/benchmarks/baseline.json
```

The tool prints a table and writes a Repr-encoded JSON report under
`.logs/benchmarks/` by default. Use `--output PATH` to select another location.
Every scenario runs in a fresh child process because Observe initialization is
process-wide and one-shot.

Bechamel estimates operation latency from monotonic-clock samples. A separate
fixed-size batch reads the OCaml GC counters for allocation and collection
rates because those monotonic counters are more reliable as direct differences
than as independent regression responders. Reported major allocation includes
promoted words; promoted allocation is also reported separately.

## Suites

- `component` isolates free-form value construction, Repr JSON projection, and
  JSON or readable formatting.
- `core` measures public logging calls with a controlled clock and no-op I/O.
  It covers filtering, routing, drain fan-out, JSON, and true-color readable
  formatting.
- `lwt-unix` initializes `Observe_lwt_unix` and performs its real clock and Unix
  standard-error write. Standard error is redirected to `/dev/null` before
  initialization, so the adapter uses its non-TTY plain style and the benchmark
  does not flood the caller's terminal.

Free-form and typed scenarios use the same small or nested logical payload.
Compare measurements only when their report metadata, benchmark configuration,
runtime, and machine are compatible. GitHub Actions stores reports as workflow
artifacts and does not enforce performance thresholds.
