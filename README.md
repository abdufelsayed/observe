# Observe

Observe is a structured logging library for OCaml. It has two basic shapes:

- A point log records one fact now.
- A wide event gathers facts during an operation and emits one record when the
  operation ends.

Point logs support text, open structured data, and declared OCaml types. Wide
events use open or typed fields and may include timestamped annotations.

```ocaml
[%observe.info text ~tag:"checkout" "checkout started"];

[%observe.info
  untyped { action = "cart_validated"; cart_id = "cart-42"; items = 2 }]
```

Observe is alpha software. Public APIs may change before 1.0.

## Install

Observe is not in the opam repository yet. Pin the alpha release from GitHub
without installing every package in the repository:

```sh
opam pin add --no-action --with-version '0.1.0~alpha1' \
  'git+https://github.com/abdufelsayed/observe.git#v0.1.0-alpha.1'
```

Install the packages your source code uses. The example in this README calls
both `Observe` and `Observe_lwt_unix`:

```sh
opam install observe observe-lwt-unix
```

Opam also installs `observe-lwt` because `observe-lwt-unix` depends on it. Add
`observe-lwt` as a direct dependency only when your code calls `Observe_lwt`.
The same rule applies to filesystem output. Code that calls
`Observe_fs_lwt_unix` should name `observe-fs-lwt-unix` too.

Add the libraries used by the executable to its Dune file:

```lisp
(executable
 (name main)
 (libraries observe observe-lwt-unix lwt.unix)
 (preprocess
  (pps observe.ppx)))
```

The PPX is optional. Every logging form also has a manual OCaml API.

## Write the first logs

Create the configuration once, then initialize Observe before the application
starts logging. `service` is the only required setting.

```ocaml
let config =
  Observe.Config.create_exn ~service:"orders" ~environment:"development" ()

let app () =
  [%observe.info text ~tag:"startup" "service ready"];
  [%observe.info
    untyped { action = "order_created"; order_id = "ord-42"; items = 3 }];
  Lwt.return_unit

let main () =
  Observe_lwt_unix.init_exn config;
  Lwt.finalize app Observe_lwt_unix.shutdown

let () = Lwt_main.run (main ())
```

In a development environment, the default console output is formatted for
people. Other environment names select NDJSON. The default level is `Info`.

Console and filesystem writers run asynchronously. `shutdown` stops new logs,
attempts to finish accepted work, and releases their resources. Use `flush`
when the program must wait for accepted work but keep logging open.

## Point logs

Use text for a sentence and open structured data for fields local to one call:

```ocaml
[%observe.warn text ~tag:"payment" "provider returned %s" code];

[%observe.info
  untyped
    {
      action = "payment_authorized";
      cart_id = "cart-42";
      provider = "example-pay";
    }]
```

Use a declared type when the event shape is shared or deserves its own name:

```ocaml
type phase = Started | Authorized of string [@@deriving observe]

type checkout = { cart_id : string; phase : phase } [@@deriving observe]

[%observe.info
  typed ~using:checkout_schema
    { cart_id = "cart-42"; phase = Authorized "auth-7" }]
```

OCaml checks the record and variant. Observe uses the generated description to
format and encode the value.

## Wide events

A wide event collects context as the operation progresses. The final log has
the operation name, a UUID, its duration, its fields, and an optional parent
reference.

```ocaml
let reserve_inventory () =
  let log = Observe.Logs.current () in
  [%observe.set log { inventory = { status = "reserved" } }];
  Lwt.return_unit

let checkout () =
  let log = Observe.Logs.current () in
  [%observe.set log { cart_id = "cart-42"; phase = "started" }];
  Observe_lwt_unix.fork ~name:"reserve-inventory" reserve_inventory

let run () =
  Observe_lwt_unix.with_operation ~name:"checkout" checkout
```

`fork` creates and emits a separate child event. The child records its parent's
name and UUID but does not copy the parent's fields.

If an ordinary exception escapes `with_operation` or `fork`, Observe adds a
structured error to that event, emits it, and raises the same exception with its
original backtrace. Lwt cancellation remains cancellation.

Use manual `create`, `set`, `set_level`, `annotate`, and `emit` when the
application needs to control the wide event's lifetime itself.

## Choose packages

The core has no scheduler, Unix, console, or filesystem dependency. Add only the
runtime and output packages needed by the program.

| Package | Use it for |
| --- | --- |
| `observe` | Logging APIs, configuration, types, formatters, drains, redaction, sampling, and capture. |
| `observe-lwt` | A custom Lwt setup with clocks, IDs, sampling, and console functions supplied by the caller. |
| `observe-lwt-unix` | Ready Lwt-Unix initialization, console output, UUIDs, scoped operations, capture, flush, and shutdown. |
| `observe-fs` | A filesystem writer over filesystem functions supplied by the caller. |
| `observe-fs-lwt` | The portable filesystem writer completed with Lwt. |
| `observe-fs-lwt-unix` | Ready daily NDJSON files on Lwt-Unix. |

`observe.ppx` is a library inside the `observe` opam package.

Opam installs package dependencies automatically. Application package metadata
should still list every package used directly. Dune files should list every
library whose modules the source references.

## Production policy

Logging calls should describe events. Keep operational policy in
`Observe.Config` during initialization:

- enrich every log with shared fields;
- cap depth, field counts, collection lengths, string sizes, and total size;
- redact exact paths or matching values;
- sample by level and retain completed errors or slow operations; and
- route selected logs to additional drains with stricter redaction.

Observe supplies the redaction tools. It does not guess that a field name or
value contains a password, token, email address, payment detail, or other secret.
The application defines those rules.

The production example configures enrichment, limits, redaction, sampling,
retention, routing, console output, and two filesystem drains:

```sh
opam exec -- dune exec examples/production.exe -- .observe/logs
```

Read the [production guide](https://abdufelsayed.github.io/observe/observe/production.html)
or open the compiled [production example](examples/production.ml).

## Things worth knowing

- A point-log author does not run when its level is rejected.
- Wide-event contributions remain lazy while the event is active. Observe
  checks the final level when the event ends.
- Every published log contains finite, immutable data. Capture, formatters, and
  drains see the same completed record unless a drain applies stricter
  redaction.
- A drain returning `Accepted` has taken responsibility for the log. It has not
  promised that formatting, writing, persistence, or remote acknowledgement has
  finished.
- Logging does not wait for console or filesystem I/O. Their queues are finite,
  and rejected or lost work is reported through diagnostics and lifecycle
  reports.
- `flush` and `shutdown` wait only for the result each output can prove. A
  completed local write does not imply `fsync` or crash durability.

## Documentation

- [Getting started](https://abdufelsayed.github.io/observe/observe/getting_started.html)
- [Point logs and wide events](https://abdufelsayed.github.io/observe/observe/logging.html)
- [Production policy](https://abdufelsayed.github.io/observe/observe/production.html)
- [Outputs and lifecycle](https://abdufelsayed.github.io/observe/observe/outputs.html)
- [Testing](https://abdufelsayed.github.io/observe/observe/testing.html)
- [Packages](https://abdufelsayed.github.io/observe/observe/packages.html)
- [API reference](https://abdufelsayed.github.io/observe/observe/Observe/index.html)
- [PPX reference](packages/core/ppx/README.md)
- [Benchmarks](bench/README.md)

## Development

```sh
opam install . --deps-only --with-test --with-doc --with-dev-setup
opam exec -- dune build @fmt @correctness @examples @doc @opam
scripts/test.sh stress
opam exec -- dune exec bench/observe_bench.exe
```

Observe is released under the [MIT License](LICENSE).
