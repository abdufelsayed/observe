# Observe

Observe is structured logging for OCaml.

It provides a portable logging core, typed and untyped structured values,
additional drains, deterministic capture, and a ready Lwt-Unix composition.
Official daily filesystem delivery is available through a separately
installable package family.
The core owns logging behavior without performing I/O or depending on a
specific runtime.

Observe supports auto-emitting point logs and manually or scope-bounded wide
logs through one logging surface. Wide logs can form causal parent-child
relationships, and separate point logs can correlate with the active wide
operation without becoming part of its accumulated event.

## What You Get

- Process-wide tagged text and structured logging.
- Open and schema-locked wide logs with incremental contributions and
  at-most-once completion.
- Explicit causal children, point-log correlation, and isolated Lwt scope.
- Scope-bounded Lwt completion that preserves results, failures, backtraces, and
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
    ()

let main () =
  [%observe.info text ~tag:"startup" "service ready"];
  Lwt.return_unit

let () =
  Observe_lwt_unix.init_exn config;
  Lwt_main.run (Lwt.finalize main Observe_lwt_unix.shutdown)
```

The default configuration admits `Info` and above. It does not guess which
application values are sensitive or activate sensitivity rules. Override only
the behavior the application needs to change:

```ocaml
let config =
  Observe.Config.create_exn ~service:"orders"
    ~min_level:Observe.Level.Debug
    ()
```

Reusable enrichers add structured context to the same completed fields as the
caller. Their callbacks remain lazy until an admitted point log is completed or
a wide log is sealed:

```ocaml
let deployment =
  Observe.Logs.Enricher.create_exn ~name:"deployment" (fun () ->
      Observe.Value.object_
        [ "region", Observe.Value.string "eu-west-1"
        ; "release", Observe.Value.string release_id
        ])

let trace =
  Observe.Logs.Enricher.create_exn ~name:"trace"
    ~authoritative_fields:["trace_id"]
    (fun () ->
      Observe.Value.object_
        ["trace_id", Observe.Value.string (current_trace_id ())])

let limits =
  Observe.Logs.Limits.create_exn
    ~max_depth:32
    ~max_object_fields:256
    ~max_collection_length:512
    ()

let config =
  Observe.Config.create_exn ~service:"orders"
    ~enrichers:[deployment; trace]
    ~limits
    ()
```

Caller-authored fields win ordinary collisions. Explicit authority applies
only to the listed non-reserved root fields; conflicting authority and duplicate
enricher names are rejected when the configuration is created. One raising,
invalid, or reserved-field contribution is omitted and diagnosed without
changing application control flow for ordinary callback failures. Enricher
callbacks are synchronous and may run concurrently for different observations,
so they must be safe for concurrent invocation. If a callback emits an admitted
Observe log, that log can invoke the same enricher again; the callback owns
termination of that recursion. Configured limit exhaustion becomes bounded
truncation or omission; runtime control exceptions propagate unchanged.
Enricher list order never chooses a collision winner.

`Observe.Logs.Limits.default` supplies finite bounds for depth, object fields,
collections, individual strings and byte values, total nodes, and deterministic
materialized size. A custom value can tighten or raise those finite bounds;
there is no unbounded mode. When safe, Observe preserves the completed prefix
and marks the affected value as truncated. The total-size value charges 32 bytes
per value node, 144 bytes plus its UTF-8 name per object field, 24 bytes per list
entry, string and byte payloads, 16 bytes plus its UTF-8 name per variant, 16
bytes for a partial truncation marker, and the UTF-8 bytes of completed-log
metadata. Metadata includes service, environment, version, text or schema
identity, operation names and identifiers, parent references, and annotation
messages. The per-string bound also covers field names, constructor names, and
metadata strings. The node bound limits both retained value nodes and productive
recursive traversal steps. This is a stable Observe accounting budget, not an
exact OCaml heap measurement.

Capture exposes truncation through `Observe.Value.view`. JSON and pretty output
render a stable `<truncated:reason>` marker. A truncated list appends that marker
after its safe prefix. A truncated object adds `_observe_truncated`; if the
caller already owns that name, Observe appends underscores until the marker name
is collision-free.

Linking `observe-lwt-unix` allocates no writer and starts no background work.
The initializer installs Lwt callback-local context, the Unix wall clock,
cryptographically random UUID v4 operation identities, and automatic output on
standard error. It is synchronous, starts no scheduler, and returns no logger
handle. When console output is active, initialization creates one bounded Lwt
writer which begins processing once a scheduler runs. All caller code emits
through the same process-wide `Observe.Logs` module. Pass `~id_generator` only
when a deterministic test or an existing identity policy needs to replace the
secure default. Observe serializes calls to that callback; each call must
promptly return a fresh, non-empty, valid UTF-8 operation ID.

`shutdown` first closes production logging admission, then drains accepted
output and stops its workers. It is terminal and idempotent. Author callbacks
are not evaluated after it begins, and a later `init` returns
`Error Observe.Runtime_closed`.

## Disclosure-safe redaction

Redaction is explicit caller policy. Omitting `~redaction` leaves authored
values unchanged; Observe does not guess from field names, values, or
environments which data is sensitive. A policy is inert until it is selected
by `Config` or by a destination drain.

Exact rules address the canonical structured value. Paths can name nested
objects, list positions, and variant cases. A typed policy can be checked once
against a schema and reused for both typed point logs and schema-locked wide
logs:

```ocaml
module R = Observe.Logs.Redaction

let typed_redaction =
  R.create_exn
    ~using:checkout_event_schema
    ~rules:
      [ R.Rule.at
          (R.Path.root
          |> R.Path.field "payment"
          |> R.Path.case "Authorized"
          |> R.Path.field "authorization_id")
          (R.Action.mask
             (R.Mask.keep_suffix ~characters:4 ~hidden:(R.Mask.Fill "*") ()))
      ; R.Rule.at
          (R.Path.root
          |> R.Path.field "payment"
          |> R.Path.case "Declined"
          |> R.Path.field "code")
          (R.Action.replace (Observe.Value.string "[payment-code]"))
      ; R.Rule.at
          (R.Path.root |> R.Path.field "item_count")
          R.Action.remove
      ]
    ()

let config =
  Observe.Config.create_exn ~service:"orders"
    ~redaction:typed_redaction
    ()
```

Known typed paths and string-mask targets are checked when the policy is built.
Custom mapped descriptions whose structural shape is intentionally opaque defer
exact lookup to the same safe runtime traversal. Unstable positions, such as a
hash table's iteration index, are not valid exact targets.

The same policy vocabulary works for open structured logs. A policy without
`~using` can target a canonical path without a schema association, which is
useful for open values and applies wherever that path exists. A value matcher
can apply a fixed replacement or a mask wherever a matching scalar occurs:

```ocaml
let global_redaction =
  R.create_exn
    ~rules:
      [ R.Rule.at (R.Path.fields [ "email" ])
          (R.Action.mask
             (R.Mask.custom ~fallback:"[email]" (fun value ->
                  String.make (String.length value) '*')))
      ; R.Rule.matching (R.Matcher.string_prefix "sk_live_")
          (R.Action.replace (Observe.Value.string "[secret]"))
      ]
    ()

let redaction =
  R.combine_exn ~policies:[ typed_redaction; global_redaction ] ()

let config =
  Observe.Config.create_exn ~service:"orders" ~redaction ()
```

`Mask.keep_prefix`, `keep_suffix`, and `keep_ends` are finite, package-
interpreted masks. They operate at UTF-8 scalar boundaries and always hide at
least one scalar from a non-empty source. `Mask.custom` receives only the
already bounded matched string. Observe contains ordinary callback failures,
invalid or over-budget results, and uses the configured fallback; it never
uses the source value as a fallback. A custom callback must terminate itself:
portable OCaml cannot preempt a callback that never returns. Custom callbacks
may run concurrently for different observations, and runtime control exceptions
propagate unchanged.

`Rule.matching` accepts replacement and masking actions, not structural
removal. String matchers also inspect point-text messages and wide annotation
messages; numeric, boolean, null, and byte matchers inspect structured scalar
values. Exact paths remain structured-only. Rules cannot rewrite package
metadata such as service, version, timestamp, level, tag, schema identity,
operation names or identifiers, parent identity, or annotation timestamps.

The effective configured policy runs after canonical materialization and before
capture, console presentation, or any drain. A matched replacement is rendered
as its ordinary safe value: JSON and pretty output do not add a redaction
wrapper. Capture can inspect `Observe.Log.redactions` for bounded effect and
location evidence without receiving the removed source value.

A destination can impose a stricter branch policy:

```ocaml
let restricted_policy =
  R.create_exn
    ~rules:
      [ R.Rule.at (R.Path.fields [ "email" ]) R.Action.remove ]
    ()

let restricted =
  Observe.Drain.create (fun log ->
      ignore (Observe.Log.fields log);
      Observe.Drain.Accepted)
  |> Observe.Drain.with_redaction ~redaction:restricted_policy

let config =
  Observe.Config.create_exn ~service:"orders"
    ~redaction ~drains:[ restricted ] ()
```

The drain receives only the already globally safe observation. Its additional
policy can remove or transform more on that branch, but cannot recover data
removed by the global policy. Conflicting exact rules are rejected when the
policy is built. Identical rules are deduplicated and rule-list order does not
choose a winner. If a dynamic matcher conflict or traversal failure remains at
runtime, only the affected value or destination branch is failed closed and a
bounded diagnostic is recorded; the raw observation is never delivered.

Identity generation is configured at the Unix composition boundary, not in the
portable core:

```ocaml
let deterministic_id =
  let next = ref 0 in
  fun () ->
    incr next;
    Printf.sprintf "test-operation-%d" !next

let () =
  Observe_lwt_unix.init_exn ~id_generator:deterministic_id config
```

The callback is mainly useful for deterministic tests or an existing identity
policy. Omitting it keeps the secure Unix UUID v4 default.

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

The directory is created recursively. The drain reserves bounded queue capacity
before projecting a log to owned compact NDJSON. One background writer then
appends accepted bytes to the UTC file selected by the log timestamp:

```text
.observe/logs/2026-08-13.jsonl
```

The writer coalesces records already queued for the same file into a reusable
write buffer; it never waits to accumulate records or crosses a flush barrier.
The queue holds at most 1,024 pending records by default. Projections in
progress occupy those slots too. A full or stopped writer rejects the newest
record before formatting it or calculating its path, without discarding
earlier acceptance. A projection that loses a shutdown race is rejected and
cannot enqueue after closure.
`Observe_lwt_unix.flush` and `shutdown` include registered filesystem workers.
Acceptance does not promise `fsync`, crash durability, retry, retention,
compression, or cross-process coordination.

Point and wide logs use the same files and worker. A completed wide log is one
flat event: searchable package metadata and consumer fields share the root
without an artificial payload envelope:

```json
{"service":"orders","timestamp":"2026-08-19T15:43:45.700000000Z","level":"info","operation":"checkout","operation_id":"2df17313-8e10-45d1-993b-247ec4c14770","duration_ms":184,"cart_id":"cart-1","phase":"completed"}
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
[%observe.info
  untyped { action = "user_login"; user_id = Observe.Type.int user_id }]
```

Declared typed schemas are the annotation-free path for variable-rich data.
`Observe.Value` remains a standalone construction and inspection surface; it
is not a second logging authoring API. Open logs use the field builder above or
the equivalent PPX object syntax.

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
  typed ~using:checkout_event_schema
    { cart_id = "cart-1"; phase = Started }]
```

Its manual expansion is:

```ocaml
Observe.Logs.info (fun m ->
  m.typed ~using:checkout_event_schema
    { cart_id = "cart-1"; phase = Started })
```

Code that does not use the deriver can construct the same validated schema
from an ordinary record description and an explicit sparse-patch builder:

```ocaml
type checkout_builder = {
  typed : checkout_event Observe.Schema.patch ->
    checkout_event Observe.Schema.patch;
  phase : phase -> checkout_event Observe.Schema.patch;
}

let checkout_event_schema =
  Observe.Schema.record checkout_event_t ~builder:(fun patch ->
    { typed = Fun.id
    ; phase =
        Observe.Schema.field patch "phase" phase_t
    })
```

`Schema` owns this public construction boundary. `Observe.Ppx_runtime` is only
the coordinated ABI used by generated code; its nested `Type`, `Schema`, and
`Logs` modules keep generated responsibilities separate. Ordinary logging code
does not construct values through it.

`[%observe.debug ...]`, `[%observe.info ...]`, `[%observe.warn ...]`, and
`[%observe.error ...]` accept text, untyped, typed, and explicit-error forms.
For a dynamic level,
use the ordinary manual function:

```ocaml
Observe.Logs.log ~level (fun m ->
  m.typed ~using:checkout_event_schema event)
```

There is no computed-level PPX form: the fixed-level extensions remain short,
while the manual call makes a runtime level explicit. `m.untyped` starts the
anonymous field builder shown above.

An explicitly supplied error can also be the complete point event:

```ocaml
[%observe.error
  error ~using:Observe.Error.exn ~backtrace exn]
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
  { cart_id = Observe.Type.string cart_id; phase = "started" }];

[%observe.set checkout
  { payment = { status = "authorized" } }];

[%observe.info checkout "payment context completed"];

Observe.Logs.set_level checkout ~level:Observe.Level.Info;
Observe.Logs.emit checkout
```

Schema-locked wide logs accept sparse typed record patches:

```ocaml
let checkout =
  Observe.Logs.create_typed ~name:"checkout"
    ~using:checkout_event_schema ()
in

Observe.Logs.set checkout (fun m ->
  m.typed (checkout_event_patch ~cart_id ~phase:Started ()));

[%observe.set checkout typed { phase = Authorized authorization_id }];
Observe.Logs.emit checkout
```

Successive object contributions merge recursively; a later scalar or variant
replaces the earlier value at that field. The first `emit` seals the lifecycle
and publishes at most once. Contributions and completion are lazy for inert or
already sealed handles. Sequential `emit` completes synchronously. If another
thread is already evaluating an admitted contribution, `emit` seals and returns
without waiting; the last admitted contributor performs completion.

Manual lifecycles accept explicit error contributions:

```ocaml
try charge () with exn ->
  let backtrace = Printexc.get_raw_backtrace () in
  Observe.Logs.set checkout (fun m ->
    m.error ~using:Observe.Error.exn ~backtrace exn);
  Printexc.raise_with_backtrace exn backtrace
```

An explicit error derives `Error` when no level was selected. An explicit
`set_level` always wins. Scope-bounded operations also capture an escaping
ordinary exception into their own wide event before re-raising the same
exception with its original backtrace. Initialization and manual wide-log
authoring install no ambient exception hook.

### Causality and point correlation

Deriving a child creates another ordinary wide log. It records the parent name
and occurrence identifier, but copies no parent event fields, schema, level, or
lifecycle:

```ocaml
let payment =
  Observe.Logs.create ~parent:checkout ~name:"capture-payment" ()
in

Observe.Logs.info ~operation:payment (fun m ->
  m.text ~tag:"payment" "capture started");

Observe.Logs.emit payment
```

The point log is still a separate auto-emitting observation. Its event carries
the payment operation name and occurrence identifier; it does not patch or emit
`payment`.

The ready Lwt runtime can own the complete operation boundary. The callback
reads the current handle, so the application logic remains top-to-bottom:

```ocaml
let checkout () =
  let checkout = Observe.Logs.current () in
  [%observe.set checkout { phase = "authorizing" }];
  [%observe.info text ~tag:"checkout" "authorizing payment"];
  authorize_payment ()

let run () =
  Observe_lwt_unix.with_operation ~name:"checkout" checkout
```

`with_operation` creates the wide log, binds it as current, and makes one final
publication attempt when the callback settles. On success it emits and returns
the exact result. An ordinary escaping exception is interpreted into this wide
log, which derives `Error` unless an explicit level wins; the same exception and
backtrace are then propagated. If custom error interpretation fails, Observe
seals and withholds the invalid wide log and diagnoses the failure instead.
`Lwt.Canceled` completes without inferred error, level, outcome, or status.

A schema-locked boundary supplies `~using` and reads it with
`Observe.Logs.current_typed ~using`. Outside a valid operation scope,
`current` raises `Observe.Logs.Current_error`; it never returns a fallback
process-wide handle.

`fork` creates a child boundary while preserving the callback outcome and an
independent child lifecycle:

```ocaml
let capture_payment () =
  let payment = Observe.Logs.current () in
  [%observe.set payment { status = "authorizing" }];
  [%observe.info payment "provider call started"];
  authorize_payment ()

let checkout () =
  Observe_lwt_unix.fork ~name:"capture-payment" capture_payment
```

The callback receives `unit` and retrieves its child with `current`. `fork`
derives the parent from the current operation and raises `Current_error
Not_bound` outside an operation scope. The child records only the parent
identity; it does not copy the parent's fields, level, schema, or lifecycle.
When it settles, the parent becomes current again. Manual detached construction
can still pass an explicit `~parent` to `Observe.Logs.create`.

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
10:23:45.700 INFO [charge-card] 71ms
  ├─ id: "55b57bde-fc11-4d4a-bf46-f1217f2617a9"
  ├─ parent: checkout (2df17313-8e10-45d1-993b-247ec4c14770)
  └─ result: Authorized
```

A correlated point stays a separate point observation and shows its operation
name and identity without becoming part of the wide event:

```text
10:23:45.650 INFO [payment] waiting for authorization
  └─ operation: checkout (2df17313-8e10-45d1-993b-247ec4c14770)
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
APIs. A raw `Repr.t` remains a Repr description: turning it into an
`Observe.Type.t` requires an Observe-owned bounded projection, normally by
composing the corresponding `Observe.Type` description or adapting one with
`Observe.Type.map`.

## Machine Output

Machine output follows the wide-event model directly. A correlated text point
is still a separate flat event:

```json
{"service":"orders","timestamp":"2026-08-19T15:43:45.650000000Z","level":"info","operation":"checkout","operation_id":"2df17313-8e10-45d1-993b-247ec4c14770","tag":"payment","message":"waiting for authorization"}
```

A child wide event includes its own identity, duration, consumer fields, and a
complete parent reference:

```json
{"service":"orders","timestamp":"2026-08-19T15:43:45.700000000Z","level":"error","operation":"charge-card","operation_id":"55b57bde-fc11-4d4a-bf46-f1217f2617a9","parent_operation":"checkout","parent_operation_id":"2df17313-8e10-45d1-993b-247ec4c14770","duration_ms":71,"result":"declined","logs":[{"timestamp":"2026-08-19T15:43:45.680000000Z","level":"warn","message":"provider retry exhausted"}]}
```

`timestamp` is RFC 3339 UTC with nanosecond precision. `duration_ms` is numeric
so ordinary JSON tools can aggregate it. The package reserves `service`,
`environment`, `version`, `timestamp`, `level`, `operation`, `operation_id`,
`parent_operation`, `parent_operation_id`, `duration_ms`, `tag`, `message`, and
`logs` at the root. Open record literals using those names fail during PPX
expansion; dynamic or typed collisions are withheld and diagnosed at the
canonical boundary. Consumer data can never overwrite package metadata.

Annotations are explicit wide-event entries. Separate point logs are never
copied into `logs`. JSON adds no universal outcome, status, start time, delivery
state, or formatted duration. `Observe.Formatter.ndjson` produces the same
object with exactly one final line feed.

## Example

The runnable example follows one checkout scenario through text and structured
point logs, manual open and schema-locked wide logs, and bounded causal Lwt
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
| `observe-lwt-unix` | Ready Lwt-Unix composition, bounded wide operations, standard-error output, and Lwt-scoped test capture. |
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
Observe_lwt_unix.Test.with_capture_exn ~config:test_config (fun capture ->
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
  match Observe.Log.kind log with
  | Observe.Log.Point { correlation } ->
      Option.map Observe.Log.operation_reference_id correlation
  | Observe.Log.Wide { operation; annotations = _ } ->
      Some (Observe.Log.operation_id operation)
```

Structured values can be traversed without parsing their JSON projection:

```ocaml
let cart_id log =
  let fields =
    match Observe.Value.view (Observe.Log.fields log) with
    | `Object fields | `Truncated_object (fields, _) -> fields
    | _ -> []
  in
  List.find_map
    (fun (name, value) ->
      match name, Observe.Value.view value with
      | "cart_id", `String id -> Some id
      | _ -> None)
    fields
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
opam exec -- dune build -p observe-fs @install @runtest
opam exec -- dune build -p observe-fs-lwt @install @runtest
opam exec -- dune build -p observe-fs-lwt-unix @install @runtest
```

Each command expects its package dependencies to be installed, matching opam's
package build environment.

The public API reference is published at
[abdufelsayed.github.io/observe](https://abdufelsayed.github.io/observe/).

## License

Observe is released under the [MIT License](LICENSE).
