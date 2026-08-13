# Observe

Observe is structured logging for OCaml.

It provides a portable logging core, typed and untyped structured values,
additional drains, deterministic capture, and a ready Lwt-Unix composition.
Official daily filesystem delivery is available through a separately
installable package family.
The core owns logging behavior without performing I/O or depending on a
specific runtime.

Observe currently ships logging only. Wide events and metrics are planned on
top of this foundation but are not part of the current public API.

## What You Get

- Process-wide tagged text and structured logging.
- Admission-first authoring callbacks shared by every message shape.
- Typed structured values with Repr machine behavior and type-aware pretty
  presentation.
- Concise logging, untyped-value, and type-description PPX syntax.
- Pretty console output with automatic truecolor, 256-color, 16-color, and
  plain fallback.
- Pure pretty, JSON, and NDJSON formatters.
- Additional application-owned drains and finite diagnostics.
- Deterministic scoped capture for tests.
- A portable completed-I/O contract, plus a ready Lwt-Unix initializer.
- Bounded daily NDJSON files through portable, Lwt, and ready Lwt-Unix
  filesystem packages.

## Installation

From a source checkout:

```sh
opam install . --deps-only --with-test --with-doc
opam exec -- dune build
```

An Lwt-Unix executable links the ready composition:

```lisp
(executable
 (name main)
 (libraries observe observe-lwt-unix lwt.unix)
 (preprocess
  (pps observe.ppx)))
```

Use only `(libraries observe)` for the portable core or for a custom I/O
composition. The PPX is optional.

Add `observe-fs-lwt-unix` when the application writes daily local files:

```lisp
(libraries observe observe-lwt-unix observe-fs-lwt-unix lwt.unix)
```

## Quick Start

Initialize Observe once at the application composition root:

```ocaml
let config =
  Observe.Config.create_exn ~service:"orders"
    ~environment:"development"
    ~min_level:Observe.Level.Debug ()

let main () =
  [%observe.info text ~tag:"startup" "service ready"];
  Lwt.return_unit

let () =
  Observe_lwt_unix.init_exn config;
  Lwt_main.run (Lwt.finalize main Observe_lwt_unix.shutdown)
```

The initializer installs Lwt callback-local context, the Unix wall clock, and
automatic output on standard error. It is synchronous, starts no scheduler,
and returns no logger handle. A bounded Lwt worker serializes accepted console
records once a scheduler runs. Application code emits through the same
process-wide `Observe.Logs` module.

`Observe.Config.Auto` uses pretty output for an absent, `dev`, or `development`
environment and NDJSON otherwise. Set `~console:Observe.Config.Pretty`,
`~console:Observe.Config.Ndjson`, or `~console:Observe.Config.Silent` to
override that selection.

## Daily Files

Create the filesystem drain before configuration and pass it with the other
application-owned drains:

```ocaml
let main () =
  let open Lwt.Syntax in
  let* filesystem =
    Observe_fs_lwt_unix.create_exn ~dir:".observe/logs" ()
  in
  let config =
    Observe.Config.create_exn ~service:"orders"
      ~drains:[filesystem] ()
  in
  Observe_lwt_unix.init_exn config;
  Lwt.finalize run_application Observe_lwt_unix.shutdown
```

The directory is created recursively. Each log is projected to owned compact
NDJSON before synchronous drain acceptance, then one bounded background writer
appends it to the UTC file selected by the log timestamp:

```text
.observe/logs/2026-08-13.jsonl
```

The writer coalesces records already queued for the same file into a reusable
write buffer; it never waits to accumulate records or crosses a flush barrier.
The queue holds at most 1,024 pending records by default. A full or stopped
writer rejects the newest record without discarding earlier acceptance.
`Observe_lwt_unix.flush` and `shutdown` include registered filesystem workers.
Acceptance does not promise `fsync`, crash durability, retry, retention,
compression, or cross-process coordination.

## Logging

Tagged text is the smallest log shape:

```ocaml
[%observe.info text ~tag:"auth" "user logged in"]
```

The extension expands to the ordinary admission-first API:

```ocaml
Observe.Logs.info (fun m ->
  m.text ~tag:"auth" "user logged in")
```

The callback runs only after route and level admission. Formatted text remains
type-safe, and rejected logs do not evaluate their formatting arguments:

```ocaml
[%observe.debug text ~tag:"query" "%s" (explain_query query)]
```

The same admitted builder owns untyped structured values:

```ocaml
[%observe.info
  untyped [%observe.value { action = "user_login"; user_id = 42 }]]
```

This expands to:

```ocaml
Observe.Logs.info (fun m ->
  m.untyped
    [%observe.value { action = "user_login"; user_id = 42 }])
```

`[%observe.value ...]` produces an `Observe.Value.t`; the outer logging
extension places it inside the admitted callback. No payload-specific deferred
variant is needed because that callback is the single deferred boundary.

OCaml values can carry an Observe type description:

```ocaml
type event = User_login of { user_id : int; method_ : string }
[@@deriving observe]

[%observe.info
  typed event_t (User_login { user_id = 42; method_ = "oauth" })]
```

Its manual expansion is:

```ocaml
Observe.Logs.info (fun m ->
  m.typed event_t (User_login { user_id = 42; method_ = "oauth" }))
```

`[%observe.debug ...]`, `[%observe.info ...]`, `[%observe.warn ...]`, and
`[%observe.error ...]` accept the same three body forms. For a dynamic level,
use `[%observe.emit (level, typed event_t event)]`; it lowers to
`Observe.Logs.emit ~level (fun m -> m.typed event_t event)`.

## Console Output

The ready Unix composition produces compact text and ordered structured trees:

```text
10:23:45.612 INFO [auth] user logged in
10:23:45.613 INFO [orders]
  ├─ action: "user_login"
  └─ user_id: 42
10:23:45.614 INFO [orders]
  └─ User_login
     ├─ user_id: 42
     └─ method_: "oauth"
```

The Unix adapter passively detects terminal color capability. Redirected
output, `NO_COLOR`, an absent or empty `TERM`, `TERM=dumb`, and failed probes
select plain output. Observe does not query or consume terminal input.

Derived descriptions preserve distinctions that JSON cannot: strings remain
quoted, ordinary constructors render as `Granted`, and polymorphic constructors
render as `` `Development``. Observe's production JSON path writes directly
from the attached type description into the formatter's existing buffer. Repr
continues to own decoding, equality, comparison, binary operations, and other
machine interoperability. Use `Observe.Type.repr` to pass an Observe
description to Repr APIs, or `Observe.Type.of_repr` to lift an existing Repr
description through the compatibility path.

## Example

The runnable example keeps initialization, tagged logs, untyped data, and
rich typed domain events in one place:

```sh
opam exec -- dune exec examples/simple.exe
```

Daily filesystem composition has a separate runnable example:

```sh
opam exec -- dune exec examples/filesystem.exe -- .observe/logs
```

It uses nested records, lists, options, ordinary variants, polymorphic variants,
and constructor payloads. The same executable is exercised by
`opam exec -- dune build @examples`.

## Packages

Observe is distributed as a small package family with explicit portable,
effect, and ready-composition boundaries:

| Package or library | Purpose |
| --- | --- |
| `observe` | Portable logging core, public authoring API, formatters, drains, diagnostics, capture, and the completed `Observe.IO` contract. |
| `observe.ppx` | Core-package sublibrary for concise logging, `[@@deriving observe]`, `[%observe.value ...]`, and embedded typed values. |
| `observe-lwt` | Lwt callback-local effects completed with caller-provided clock and console functions. |
| `observe-lwt-unix` | Ready Lwt-Unix composition, standard-error output, and Lwt-scoped test capture. |
| `observe-fs` | Portable daily filename, NDJSON projection, bounded worker state, barriers, and failure behavior over injected I/O. |
| `observe-fs-lwt` | Lwt completion for application-owned or Mirage filesystem capabilities, without Unix dependencies. |
| `observe-fs-lwt-unix` | Ready recursive-directory setup, append-only daily files, and registration with the Lwt-Unix lifecycle. |

The core does not depend on Lwt or Unix. `Observe.Make (IO)` accepts one
completed I/O implementation; `Observe_lwt_unix` is the ready path for
ordinary Lwt applications. Call `Observe_lwt_unix.flush ()` for a sequence
barrier or `Observe_lwt_unix.shutdown ()` before process exit.

## Drains And Capture

Automatic console output is the built-in formatted output. On Unix it writes to
standard error, which may be a terminal, file, or pipe. Configured
`Observe.Drain.t` values are additional outputs that receive completed
`Observe.Log.t` values and may apply `Observe.Formatter.pretty`, `.json`,
`.ndjson`, or a custom pure formatter. Official filesystem delivery uses the
conventional `.jsonl` filename extension; that storage name does not change the
format's public name. Its asynchronous failure signal is finite and
non-recursive; later offers reject while other drains and application control
flow continue independently.

Tests can capture ordinary `Observe.Logs` calls without resetting global
production state:

```ocaml
Observe_lwt_unix.Test.with_capture_exn test_config (fun capture ->
    let open Lwt.Syntax in
    let* () = My_library.run () in
    let logs = Observe.Capture.logs capture in
    Lwt.return logs)
```

Capture is finite and deterministic. Typed OCaml values remain retained by
reference rather than becoming deep snapshots.

## Development

```sh
opam install . --deps-only --with-test --with-doc --with-dev-setup
opam exec -- dune build @fmt @correctness @examples @doc @opam
scripts/test.sh stress
opam exec -- dune exec bench/observe_bench.exe
```

Use `scripts/test.sh quick` when the correctness run should leave a durable
report. All runner profiles record their workload controls; correctness and
stress failures also copy Alcotest outputs under the ignored `.logs/`
directory. The separate benchmark tool records informational measurements and
never participates in correctness or stress gates. See
[`bench/README.md`](bench/README.md) for its suites and output contract.

CI also proves each installable package independently:

```sh
opam exec -- dune build -p observe @install @runtest
opam exec -- dune build -p observe-lwt @install @runtest
opam exec -- dune build -p observe-lwt-unix @install @runtest
```

The latter two commands expect their package dependencies to be installed,
matching opam's package build environment.

The public API reference is published at
[abdufelsayed.github.io/observe](https://abdufelsayed.github.io/observe/).

## License

Observe is released under the [MIT License](LICENSE).
