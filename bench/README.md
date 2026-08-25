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
  --scenario core/canonical/open-point
opam exec -- dune exec bench/observe_bench.exe -- --suite fs
opam exec -- dune exec bench/observe_bench.exe -- --suite fs-lwt-unix
opam exec -- dune exec bench/observe_bench.exe -- \
  --compare .logs/benchmarks/baseline.json
```

The tool prints a table and writes a compact typed JSON report under
`.logs/benchmarks/` by default. Use `--output PATH` to select another location.
Every scenario runs in a fresh child process because Observe initialization is
process-wide and one-shot.

Bechamel estimates operation latency from monotonic-clock samples. A separate
fixed-size batch reads the OCaml GC counters for allocation and collection
rates because those monotonic counters are more reliable as direct differences
than as independent regression responders. Reported major allocation includes
promoted words; promoted allocation is also reported separately.

Canonical point and wide scenarios also retain the latest completed `Log.t`
through their benchmark drain and report its reachable heap size. This is a
structural retained-size estimate for the immutable completed observation, not
an operating-system resident-memory measurement. Other scenarios report
retained size as unavailable.

Formatter scenarios report the stable encoded byte count beside latency,
throughput, and allocation. Other boundaries report encoded size as
unavailable because console and filesystem byte ownership is asynchronous or
amortized. Report schema version 3 adds this field.

## Suites

- `component` isolates untyped value construction, typed JSON projection,
  point and wide capture, and point, root-wide, and child-wide JSON, NDJSON,
  or pretty formatting.
- `core` measures public logging calls with a controlled clock and no-op I/O.
  It covers filtering, routing, drain fan-out, JSON, true-color pretty
  formatting, complete typed canonical points, anonymous point fields, open
  wide fragments, typed sparse patches, and nested
  typed patches. Operation scenarios separately measure open and typed current
  lookup, bounded success and failure, a point inside an operation, and child
  execution together with its required parent lifecycle. Enrichment scenarios
  measure zero, one, multiple, and many configured enrichers. Materialization
  scenarios cover a representative default payload, depth, object width,
  collection length, node count, embedded typed values, byte and string
  truncation, and a total-byte budget that withholds the observation. The
  boundary probes use deliberately small limits so their behavior is cheap to
  measure; they are not substitutes for correctness tests. These boundaries are
  named separately in the report; no latency number is treated as proof of
  semantic correctness or lock-freedom.
- `fs` isolates prompt rejection by a full or closed portable filesystem
  writer. Its controlled blocked writer keeps the rejection boundary separate
  from accepted projection and delivery.
- `lwt-unix` initializes `Observe_lwt_unix` and performs its real clock and Unix
  standard-error write. Each measured operation includes a `flush` sequence
  barrier, so it measures formatting, bounded submission, scheduler wakeup,
  and completed delivery rather than a full queue's rejection path. Standard
  error is redirected to `/dev/null` before initialization, so the adapter uses
  its non-TTY plain style and does not flood the caller's terminal. Point and
  wide JSON and pretty scenarios use the same worker and flush boundary.
- `fs-lwt-unix` writes to a fresh temporary directory through the ready
  filesystem package. `completed` scenarios include one log and one shared
  lifecycle flush. `batch-100` scenarios write 100 logs and flush once; their
  latency, throughput, and allocation are normalized per logical record. The
  filesystem uses ordinary append writes into the operating-system page cache;
  point and wide records use the same writer. These measurements do not include
  `fsync` and make no durability claim.

Untyped and typed scenarios use the same small or nested logical event.
Compare measurements only when their report metadata, benchmark configuration,
runtime, and machine are compatible. GitHub Actions stores reports as workflow
artifacts and does not enforce performance thresholds. The finite defaults
preserve Observe's established canonical safety envelope: depth 64, 1,024
object fields or collection entries, 1 MiB per string or byte value, 100,000
nodes, and 4 MiB of deterministic total-size accounting. The default scenarios
measure representative work below that envelope, while controlled low-limit
scenarios measure localized truncation and full withholding. This evidence
supports a bounded compatibility-preserving default; it does not claim those
numbers are optimal or universally safe for every workload. Applications can
select another validated finite policy from their own workload evidence.
