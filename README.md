# Observe

Observe is structured logging for OCaml.

It provides a portable logging core, typed and untyped structured values,
additional drains, deterministic capture, and a ready Lwt-Unix composition.
Official daily filesystem delivery is available through a separately
installable package family.
The core owns logging behavior without performing I/O or depending on a
specific runtime.

Observe supports auto-emitting point logs and manually or deliberately managed
wide logs through one logging surface. Wide logs can form causal parent-child
relationships, and separate point logs can correlate with the active wide
operation without becoming part of its accumulated body.

## What You Get

- Process-wide tagged text and structured logging.
- Open and schema-locked wide logs with incremental contributions and
  at-most-once completion.
- Explicit causal children, point-log correlation, and isolated Lwt scope.
- Managed Lwt completion that preserves results, failures, backtraces, and
  cancellation.
- Admission-first authoring callbacks shared by every message shape.
- Typed structured values with Repr machine behavior and type-aware pretty
  presentation.
- Concise logging, untyped-value, and type-description PPX syntax.
- Pretty console output with automatic truecolor, 256-color, 16-color, and
  plain fallback.
- Pure pretty, JSON, and NDJSON formatters.
- Additional composition-root drains and finite diagnostics.
- Deterministic scoped capture for tests.
- One bounded immutable completed observation shared by capture and every
  output branch.
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

Add `observe-fs-lwt-unix` when the program writes daily local files:

```lisp
(libraries observe observe-lwt-unix observe-fs-lwt-unix lwt.unix)
```

## Quick Start

Initialize Observe once at the process composition root:

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
records once a scheduler runs. All caller code emits through the same
process-wide `Observe.Logs` module.

`Observe.Config.Auto` uses pretty output for an absent, `dev`, or `development`
environment and NDJSON otherwise. Set `~console:Observe.Config.Pretty`,
`~console:Observe.Config.Ndjson`, or `~console:Observe.Config.Silent` to
override that selection.

## Daily Files

Create the filesystem drain before configuration and pass it with the other
composition-root drains:

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

Point and wide logs use the same files and worker. A completed wide log keeps
consumer fields under `body` and package metadata under `operation`:

```json
{"service":"orders","timestamp":"1787120625700000000","level":"info","operation":{"name":"checkout","id":"op_01J...","duration_ns":"184000000"},"body":{"cart_id":"cart-1","phase":"completed"}}
```

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

The same admitted builder owns anonymous structured fields:

```ocaml
[%observe.info
  untyped { action = "user_login"; user_id = 42 }]
```

The PPX recognizes self-describing literals, lists, options, and nested
anonymous objects. It does not require a nested value extension or field
descriptions for that syntax. The equivalent manual builder remains explicit:

```ocaml
Observe.Logs.info (fun m ->
  let open Observe.Logs in
  m.untyped
  |+ m.field "action" Observe.Type.string "user_login"
  |+ m.field "user_id" Observe.Type.int 42
  |> m.seal)
```

Every manual field carries its normal `Observe.Type.t` description. An
arbitrary expression in an open PPX object also supplies its description
because PPX expansion occurs before OCaml type checking:

```ocaml
let user_id = current_user_id () in
[%observe.info untyped { action = "user_login"; user_id = int user_id }]
```

Declared typed schemas are the annotation-free path for variable-rich data.
`Observe.Value` remains available as an explicit compatibility and inspection
surface through `m.value`.

Declared point logs have record roots. Deriving a record produces its complete
description, schema witness, and sparse patch authoring support:

```ocaml
type phase = Started | Authorized of string [@@deriving observe]

type checkout_event = {
  cart_id : string;
  phase : phase;
}
[@@deriving observe]

[%observe.info
  typed checkout_event_schema { cart_id = "cart-1"; phase = Started }]
```

Its manual expansion is:

```ocaml
Observe.Logs.info (fun m ->
  m.typed checkout_event_schema { cart_id = "cart-1"; phase = Started })
```

`[%observe.debug ...]`, `[%observe.info ...]`, `[%observe.warn ...]`, and
`[%observe.error ...]` accept the same three body forms. For a dynamic level,
use `[%observe.emit (level, typed checkout_event_schema event)]`; it lowers to
`Observe.Logs.log ~level (fun m -> m.typed checkout_event_schema event)`.

For code written against E1, fixed-level calls and `[%observe.emit]` keep their
meaning. The two manual migrations are mechanical: use `Logs.log ~level`
instead of the former computed-level `Logs.emit ~level`, and use `m.value value`
for an already constructed `Observe.Value.t` instead of the former
`m.untyped value`. The name `m.untyped` now starts the shared anonymous-field
builder shown above.

An explicitly supplied error can also be the complete point event:

```ocaml
[%observe.error error Observe.Error.exn ~backtrace exn]
```

This interprets only the value supplied by the caller; it does not install an
exception hook or catch ambient failures.

## Wide Logs

A point log is one automatically emitted event. A wide log is the same
structured logging meaning accumulated across several manual contributions and
emitted once:

```ocaml
let checkout = Observe.Logs.create ~name:"checkout" () in

[%observe.set checkout
  untyped { cart_id = string cart_id; phase = "started" }];

[%observe.set checkout
  untyped { payment = { status = "authorized" } }];

Observe.Logs.set_level checkout Observe.Level.Info;
Observe.Logs.emit checkout
```

Schema-locked wide logs accept sparse typed record patches:

```ocaml
let checkout =
  Observe.Logs.create_typed ~name:"checkout" checkout_event_schema
in

Observe.Logs.set checkout (fun m ->
  m.typed (checkout_event_patch ~cart_id ~phase:Started ()));

[%observe.set checkout { phase = Authorized authorization_id }];
Observe.Logs.emit checkout
```

Successive object contributions merge recursively; a later scalar or variant
replaces the earlier value at that field. The first `emit` seals the lifecycle
and publishes at most once. Contributions and completion are lazy for inert or
already sealed handles.

Errors are explicit contributions, not automatic exception tracking:

```ocaml
try charge () with exn ->
  let backtrace = Printexc.get_raw_backtrace () in
  Observe.Logs.set checkout (fun m ->
    m.error Observe.Error.exn ~backtrace exn);
  Printexc.raise_with_backtrace exn backtrace
```

An explicit error derives `Error` when no level was selected. An explicit
`set_level` always wins. Observe records errors supplied by the caller or an
error escaping a deliberately invoked managed boundary; initialization and
scope-only binding catch nothing.

### Causality and point correlation

Deriving a child creates another ordinary wide log. It copies only the parent
occurrence identifier—not the parent body, schema, level, or lifecycle:

```ocaml
let payment =
  Observe.Logs.create ~parent:checkout ~name:"capture-payment" ()
in

Observe.Logs.info ~operation:payment (fun m ->
  m.text ~tag:"payment" "capture started");

Observe.Logs.emit payment
```

The point log is still a separate auto-emitting observation. Its envelope
carries the payment occurrence identifier; it does not patch or emit
`payment`.

The ready Lwt runtime can bind the same handle for automatic point correlation:

```ocaml
Observe_lwt_unix.with_wide checkout (fun () ->
  [%observe.info text ~tag:"checkout" "authorizing payment"];
  authorize_payment ())
```

`with_wide` is scope only. It neither catches the callback's exception nor
emits `checkout`. Nested scopes restore their parent, concurrent scopes remain
isolated, and there is no fallback current operation outside a valid scope.

### Managed Lwt completion

Use `manage` only when the supplied callback is the real boundary being
observed:

```ocaml
let checkout = Observe.Logs.create ~name:"checkout" () in

Observe_lwt_unix.manage checkout ~error:Observe.Error.exn (fun () ->
  let open Lwt.Syntax in
  let* authorization = authorize_payment () in
  [%observe.set checkout
    untyped { payment = { authorization_id = string authorization.id } }];
  Lwt.return authorization)
```

On success, `manage` emits once and returns the exact result. An ordinary
escaping exception is interpreted into this wide log, which derives `Error`
unless an explicit level wins; the same exception and backtrace are then
propagated. `Lwt.Canceled` also completes once but contributes no inferred
error, level, outcome, or status.

A managed child keeps Evlog's useful callback shape while preserving the
callback outcome and independent child lifecycle:

```ocaml
Observe_lwt_unix.fork ~parent:checkout ~name:"capture-payment"
  ~error:Observe.Error.exn (fun payment ->
    let open Lwt.Syntax in
    let* authorization = authorize_payment () in
    [%observe.set payment
      untyped { authorization_id = string authorization.id }];
    Lwt.return authorization)
```

For a stream whose true terminal boundary occurs after its handler returns,
the integration owns a single-use terminal token instead of calling `manage`
at the handler boundary:

```ocaml
let terminal =
  Observe.Logs.Terminal.create ~error:Observe.Error.exn response
in

on_close (fun status ->
  Observe.Logs.Terminal.complete terminal
    ~set:(fun m ->
      let open Observe.Logs in
      m.untyped
      |+ m.field "status" Observe.Type.int status
      |> m.seal)
    ());
on_error (fun exn backtrace ->
  Observe.Logs.Terminal.fail terminal ~backtrace exn);
on_cancel (fun () -> Observe.Logs.Terminal.cancel terminal ())
```

Only the first terminal action wins. Its optional `~set` callback uses the same
lazy sparse-patch API and contributes protocol status or other terminal facts
before canonical completion. Losing callbacks author nothing.

## Console Output

The ready Unix composition produces compact text and ordered structured trees:

```text
10:23:45.612 INFO [auth] user logged in
10:23:45.613 INFO [orders]
  ├─ action: "user_login"
  └─ user_id: 42
10:23:45.614 INFO [orders]
  ├─ cart_id: "cart-1"
  └─ phase: Started
10:23:45.700 INFO [charge-card] 71ms (op_child <- op_parent)
  └─ result: Authorized
```

A correlated point stays a separate point observation and shows its operation
identity without becoming part of the wide body:

```text
10:23:45.650 INFO [payment] (op_parent) waiting for authorization
```

The Unix adapter passively detects terminal color capability. Redirected
output, `NO_COLOR`, an absent or empty `TERM`, `TERM=dumb`, and failed probes
select plain output. Observe does not query or consume terminal input.

Derived descriptions and the immutable snapshot preserve distinctions that
JSON cannot: strings remain
quoted, ordinary constructors render as `Granted`, and polymorphic constructors
render as `` `Development``. The attached type description freezes admitted
authoring directly into that format-neutral snapshot; JSON, pretty output,
capture, and drains then consume the same completed value. Repr continues to
own decoding, equality, comparison, binary operations, and other machine
interoperability. Use `Observe.Type.repr` to pass an Observe description to Repr
APIs. `Observe.Type.of_repr` remains useful for direct Repr interoperability,
but an opaque lifted description cannot enter a completed log unless Observe
has a bounded package-owned freezer for it; there is no live or unbounded
fallback.

## Machine Output

Uncorrelated point JSON retains the E1 envelope. A correlated point adds only
`operation_id`:

```json
{"service":"orders","timestamp":"1787120625660000000","level":"info","operation_id":"op_parent","body":{"kind":"inventory_wait"}}
```

A wide log uses a nested collision-free operation envelope. A child adds
`parent_id` inside that envelope:

```json
{"service":"orders","timestamp":"1787120625771000000","level":"info","operation":{"name":"charge-card","id":"op_child","parent_id":"op_parent","duration_ns":"71000000"},"body":{"result":"authorized"}}
```

`timestamp` and `duration_ns` are exact decimal nanosecond strings. JSON adds
no inferred outcome, status, start time, progress-log array, delivery state, or
formatted duration. `Observe.Formatter.ndjson` produces the same object with
exactly one final line feed.

## Example

The runnable example follows one checkout scenario through text and structured
point logs, manual open and schema-locked wide logs, and managed causal Lwt
work:

```sh
opam exec -- dune exec examples/simple.exe
```

Daily filesystem composition has a separate runnable example:

```sh
opam exec -- dune exec examples/filesystem.exe -- .observe/logs
```

It shows annotation-free anonymous literals, nested objects, lists, options,
ordinary variants, and constructor payloads. The same executable is exercised
by `opam exec -- dune build @examples`.

## Packages

Observe is distributed as a small package family with explicit portable,
effect, and ready-composition boundaries:

| Package or library | Purpose |
| --- | --- |
| `observe` | Portable logging core, public authoring API, formatters, drains, diagnostics, capture, and the completed `Observe.IO` contract. |
| `observe.ppx` | Core-package sublibrary for concise logging, `[@@deriving observe]`, `[%observe.value ...]`, and embedded typed values. |
| `observe-lwt` | Lwt callback-local scope and outcome effects completed with caller-provided clock and console functions. |
| `observe-lwt-unix` | Ready Lwt-Unix composition, managed wide execution, standard-error output, and Lwt-scoped test capture. |
| `observe-fs` | Portable daily filename, NDJSON projection, bounded worker state, barriers, and failure behavior over injected I/O. |
| `observe-fs-lwt` | Lwt completion for caller-provided filesystem capabilities, including Mirage capabilities, without Unix dependencies. |
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

Capture is finite and deterministic. Every retained `Log.t` is the same
bounded immutable completed observation offered to console and drains; caller
mutation after emission cannot change later capture or output.

Consumers inspect semantic values directly instead of parsing presentation
output:

```ocaml
let inspect log =
  match Observe.Log.kind log, Observe.Log.operation log with
  | Observe.Log.Point, None ->
      Observe.Log.correlation_id log
  | Observe.Log.Wide, Some operation ->
      Some (Observe.Log.operation_id operation)
  | Observe.Log.Point, Some _ | Observe.Log.Wide, None ->
      assert false
```

A third-party formatter or drain receives this same public immutable value:

```ocaml
let formatter =
  Observe.Formatter.create (fun log ->
    inspect log |> ignore;
    Observe.Formatter.format Observe.Formatter.json log)

let drain =
  Observe.Drain.create (fun log ->
    inspect log |> ignore;
    Observe.Drain.Accepted)
```

Drain acceptance means only that the drain took immediate ownership. It does
not mean that formatting, queuing, flushing, persistence, or acknowledgement
completed.

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
never participates in correctness or stress gates. Its report separates point
and wide capture, formatting, drain, runtime, and filesystem boundaries and
records encoded byte size where a formatter owns a stable byte result. See
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
