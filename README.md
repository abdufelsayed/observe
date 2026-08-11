# Observe

Observe is structured logging for OCaml.

It provides a portable logging core, typed and free-form structured values,
additional drains, deterministic capture, and a ready Lwt-Unix composition.
The core owns logging behavior without performing I/O or depending on a
specific runtime.

Observe currently ships logging only. Wide events and metrics are planned on
top of this foundation but are not part of the current public API.

## What You Get

- Process-wide tagged text and structured logging.
- Deferred messages and values that run only after admission.
- Typed structured values backed by Repr.
- A concise PPX for free-form values and Repr descriptions.
- Readable terminal output with automatic truecolor, 256-color, 16-color, and
  plain fallback.
- Pure readable, JSON, and JSON Lines formatters.
- Additional application-owned drains and finite diagnostics.
- Deterministic scoped capture for tests.
- Portable runtime and platform contracts, plus a ready Lwt-Unix initializer.

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
 (libraries observe observe.lwt-unix lwt.unix)
 (preprocess
  (pps observe.ppx)))
```

Use only `(libraries observe)` for the portable core or for a custom runtime
and platform composition. The PPX is optional.

## Quick Start

Initialize Observe once at the application composition root:

```ocaml
let config =
  Observe.Config.create_exn ~service:"orders"
    ~min_level:Observe.Level.Debug ()

let () = Observe_lwt_unix.init_exn config

let () =
  Observe.Logs.info
    (Observe.Logs.text ~tag:"startup" "service ready")
```

The initializer installs Lwt callback-local context, the Unix wall clock, and
automatic readable output on standard error. It is synchronous, starts no
scheduler, and returns no logger handle. Application code emits through the
same process-wide `Observe.Logs` module.

## Logging

Tagged text is the smallest log shape:

```ocaml
Observe.Logs.info
  (Observe.Logs.text ~tag:"auth" "user logged in")
```

Expensive text can be deferred until after level admission:

```ocaml
Observe.Logs.debug
  (Observe.Logs.text_lazy ~tag:"query" (fun () -> explain_query query))
```

The PPX constructs free-form structured values lazily:

```ocaml
Observe.Logs.info
  (Observe.Logs.free
     [%observe.value { action = "user_login"; user_id = 42 }])
```

OCaml values can carry a Repr description:

```ocaml
type event = User_login of { user_id : int; method_ : string }
[@@deriving observe]

Observe.Logs.info
  (Observe.Logs.structured event_t
     (User_login { user_id = 42; method_ = "oauth" }))
```

## Terminal Output

The ready Unix composition produces compact text and ordered structured trees:

```text
10:23:45.612 INFO [auth] user logged in
10:23:45.613 INFO [orders]
  ├─ action: user_login
  └─ user_id: 42
10:23:45.614 INFO [orders]
  └─ User_login
     ├─ user_id: 42
     └─ method_: oauth
```

The Unix adapter passively detects terminal color capability. Redirected
output, `NO_COLOR`, `TERM=dumb`, and failed probes select plain output. Observe
does not query or consume terminal input.

## Libraries

Observe is currently distributed as one opam package with independently
composable Dune libraries:

| Library | Purpose |
| --- | --- |
| `observe` | Portable logging core, public authoring API, formatters, drains, diagnostics, capture, and runtime/platform contracts. |
| `observe.ppx` | `[@@deriving observe]`, `[%observe.value ...]`, and embedded typed values. |
| `observe.lwt` | Lwt callback-local runtime mechanism. |
| `observe.unix` | Unix clock and standard-error platform mechanism. |
| `observe.lwt-unix` | Ready composition and Lwt-scoped test capture. |

The core does not depend on Lwt or Unix. `Observe.Runtime.Make` composes any
compatible runtime and platform; `Observe_lwt_unix` is the ready path for
ordinary Lwt applications.

## Drains And Capture

The platform terminal is the automatic formatted output. Configured
`Observe.Drain.t` values are additional outputs that receive completed
`Observe.Log.t` values and may apply `Observe.Formatter.readable`, `.json`,
`.json_lines`, or a custom pure formatter.

Tests can capture ordinary `Observe.Logs` calls without resetting global
production state:

```ocaml
Observe_lwt_unix.Test.with_capture test_config (fun capture ->
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
opam exec -- dune build @stress
```

The public API reference is published at
[abdufelsayed.github.io/observe](https://abdufelsayed.github.io/observe/).

## License

Observe is released under the [MIT License](LICENSE).
