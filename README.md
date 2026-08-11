# Observe

Observe is structured logging for OCaml with a portable core and a ready
Lwt-Unix composition. The core owns admission, formatting, diagnostics,
additional drains, and deterministic capture without performing I/O. Runtime
adapters provide dynamic context; platform adapters provide wall-clock time and
terminal output.

The package currently ships logging only: tagged text, free-form values, typed
Repr-backed values, level filtering, terminal formatting, drains, diagnostics,
and capture. It does not currently ship wide-event or metric APIs.

## Installation

From a source checkout, install the package with opam:

```sh
opam install . --deps-only --with-test --with-doc
opam install .
```

Released packages will use the package name `observe`.

An Lwt-Unix application uses the ready composition:

```lisp
(executable
 (name main)
 (libraries observe observe.lwt-unix lwt.unix)
 (preprocess
  (pps observe.ppx)))
```

Use only `(libraries observe)` for the portable core or a custom runtime and
platform composition. `(libraries observe)` is also sufficient when a program constructs
`Observe.Value.t` and `Observe.Type.t` values without PPX. Add
`(pps observe.ppx)` for `[@@deriving observe]` and
`[%observe.value ...]`.

## Ready Lwt-Unix initialization

Construct one configuration and initialize Observe once at the application
composition root:

```ocaml
let config = Observe.Config.create_exn ~service:"orders" ()

let () = Observe_lwt_unix.init_exn config

let () =
  Observe.Logs.info
    (Observe.Logs.text ~tag:"startup" "service ready")
```

The initializer installs Lwt callback-local context, the OS wall clock, and
automatic formatted output on standard error. It returns no logger or runtime
handle. Every linked use of `Observe.Logs` consults the same core-owned active
system.

`Observe_lwt_unix.init` returns an `Observe.Runtime.init_error` result.
`init_exn` raises `Observe.Runtime.Init_error` with the same error. Both are
synchronous and neither calls `Lwt_main.run`. The executable remains
responsible for starting the Lwt scheduler.

With the default readable presentation, tagged text is compact and structured
values use an ordered tree:

```text
10:23:45.612 INFO [startup] service ready
10:23:45.613 INFO [orders]
  ├─ action: user_login
  └─ user_id: 42
10:23:45.614 INFO [orders]
  └─ User_login
     ├─ user_id: 42
     └─ method_: oauth
```

The timestamp is UTC with millisecond precision. Every tagged-text record uses
the same `time LEVEL [tag] message` grammar. An interactive standard-error
terminal receives one semantic color palette rendered at the best detected
truecolor, 256-color, or 16-color capability. A redirected terminal,
`NO_COLOR`, or `TERM=dumb` selects the same presentation without control
sequences.

## Authoring logs

Ordinary code emits through the process-wide `Observe.Logs` API. A runtime and
platform composition determines the active capture or production route.

Text logs always carry a tag:

```ocaml
Observe.Logs.info
  (Observe.Logs.text ~tag:"auth" "User logged in")
```

Defer expensive or effectful text until after level admission:

```ocaml
Observe.Logs.debug
  (Observe.Logs.text_lazy ~tag:"query" (fun () -> explain_query query))
```

Free-form values are also constructed after admission:

```ocaml
Observe.Logs.info
  (Observe.Logs.free (fun () ->
       Observe.Value.object_
         [
           ("action", Observe.Value.string "user_login");
           ("user_id", Observe.Value.int 42);
         ]))
```

The PPX provides a shorter deferred form:

```ocaml
Observe.Logs.info
  (Observe.Logs.free
     [%observe.value { action = "user_login"; user_id = 42 }])
```

Typed logs retain an OCaml value together with its Repr description:

```ocaml
type event = User_login of { user_id : int; method_ : string }
[@@deriving observe]

Observe.Logs.info
  (Observe.Logs.structured event_t
     (User_login { user_id = 42; method_ = "oauth" }))
```

Use `Observe.Logs.emit ~level` when the level is computed rather than fixed:

```ocaml
Observe.Logs.emit ~level
  (Observe.Logs.text ~tag:"worker" message)
```

Before production initialization, logs outside an active capture are withheld
and counted as `Observe.Diagnostics.Not_initialized`; logging does not raise
merely because initialization has not happened.

## Expert runtime composition

The core exposes runtime and platform contracts for other compatible
compositions. This complete direct-style example captures four logs without
installing production terminal output:

```ocaml
module Direct_runtime = struct
  type 'a t = 'a
  type context = unit
  type 'a key = 'a option ref

  let return value = value
  let bind value f = f value
  let create_key () = ref None
  let get () key = !key

  let with_binding () key value callback =
    let previous = !key in
    key := Some value;
    Fun.protect ~finally:(fun () -> key := previous) callback

  let protect () ~finally callback = Fun.protect ~finally callback
  let is_control_exception () _ = false
end

module Stdlib_platform = struct
  type t = unit

  let terminal_style () = Observe.Formatter.Plain

  let now () =
    Ok (Observe.Instant.of_epoch_nanoseconds 1_750_000_000_000_000_000L)

  let write_terminal () record =
    output_string stderr record;
    flush stderr;
    Observe.Platform.Accepted
end

module Observe_direct =
  Observe.Runtime.Make (Direct_runtime) (Stdlib_platform)

type event = User_login of { user_id : int; method_ : string }
[@@deriving observe]

let runtime =
  Observe_direct.create ~runtime_context:() ~platform:()

let config =
  Observe.Config.create_exn ~service:"capture-example"
    ~min_level:Observe.Level.Debug ()

let capture () =
  Observe_direct.with_capture runtime config (fun capture ->
      Observe.Logs.info
        (Observe.Logs.text ~tag:"auth" "User logged in");
      Observe.Logs.debug
        (Observe.Logs.text_lazy ~tag:"query" (fun () -> "plan=by-id"));
      Observe.Logs.info
        (Observe.Logs.free
           [%observe.value { action = "user_login"; user_id = 42 }]);
      Observe.Logs.info
        (Observe.Logs.structured event_t
           (User_login { user_id = 42; method_ = "oauth" }));
      Observe.Capture.logs capture)

let () =
  match capture () with
  | Ok logs -> Printf.printf "captured %d logs\n" (List.length logs)
  | Error Observe.Runtime.Runtime_already_registered ->
      failwith "another Observe runtime is already registered"
  | Error (Observe.Runtime.Invalid_capacity capacity) ->
      invalid_arg (Printf.sprintf "invalid capture capacity: %d" capacity)
```

Use `(libraries observe)` plus `observe.ppx` for this example. A capture is the innermost
dynamic route: it suppresses production terminal and drain delivery during the
callback, restores any previous route afterward, and closes exactly once on
return, exception, or native cancellation. A retained capture remains readable
afterward, but it no longer accepts logs.

## Configuration and initialization

`Observe.Config.create` validates the service, environment, and version strings
and returns `(config, error) result`. `Observe.Config.create_exn` raises
`Observe.Config.Invalid_configuration error` instead.

```ocaml
let config_result =
  Observe.Config.create ~service:"orders" ~environment:"production"
    ~version:"2026.08" ~min_level:Observe.Level.Info ()

let config =
  match config_result with
  | Ok config -> config
  | Error error ->
      Format.eprintf "invalid Observe configuration: %a@."
        Observe.Config.pp_error error;
      exit 2
```

The approved configuration fields have distinct responsibilities:

- `enabled` controls admission to every output.
- `min_level` controls level admission.
- `pretty` selects readable rather than structured terminal formatting.
- `silent` suppresses automatic terminal output, not additional drains.
- `drains` supplies additional application-owned outputs.
- `service`, `environment`, and `version` become completed-log metadata.

The first successful initialization owns the process route. A second
initialization returns `Already_initialized`, and a different registered
runtime cannot replace the owner.

## Terminal output and drains

The terminal and drains are deliberately different roles.

- The Unix platform adapter writes the automatic terminal path to standard
  error. It reports the maximum color capability derived from passive terminal
  signals. The core selects and runs the terminal formatter, including styling
  and record termination, then supplies one complete string to
  `write_terminal`. Detection does not query or consume terminal input.
- Configured drains receive the completed `Observe.Log.t` directly. They can
  apply `Observe.Formatter.readable Observe.Formatter.Plain`, `.json`,
  `.json_lines`, or a custom pure formatter before handing data to an
  application-owned sink.
- `silent` disables only automatic terminal delivery. Admission and configured
  drains remain active.
- `Accepted` means immediate handoff only. It does not promise flushing,
  durability, ordering, retry, or shutdown.

The standard-error write is synchronous because `Observe.Logs` emission
returns `unit`. A slow redirected destination can delay the calling Lwt
callback. Observe does not add a queue or background writer in this release.

A drain callback is synchronous. If it schedules work that outlives the
callback, it must first copy or project every part of the log that work needs.

## Ownership and safety boundaries

- The core package performs no I/O. `Observe.Runtime.S` owns dynamic binding,
  cleanup, and cancellation preservation; `Observe.Platform.S` owns wall-clock
  access and terminal writes.
- Ordinary authoring, formatter, drain, clock, and terminal callback failures
  encountered during logging are contained and counted in diagnostics rather
  than escaping `Observe.Logs` emission. Runtime control exceptions identified
  by `Runtime.S.is_control_exception` remain native and are not converted into
  logging failures.
- `Observe.Value.embed` and `Observe.Logs.structured` retain typed OCaml values
  by reference. Capture is not a deep snapshot. Do not mutate retained values
  while a later formatter or drain must observe stable data.
- Arbitrary Repr descriptions do not currently guarantee bounded traversal,
  cycle-safe rendering, or redaction. Application code owns those policies for
  supplied values and custom formatters.
- Process diagnostics are finite, saturating, non-clearing counters. Capture
  has finite capacity and reports overflow through capture-local diagnostics.
- Observe creates no background workers and owns no exporter, flush, or
  shutdown lifecycle.

## Lwt-scoped capture

Tests can capture the ordinary static API without production initialization or
a global reset:

```ocaml
let test_config =
  Observe.Config.create_exn ~service:"library-test"
    ~min_level:Observe.Level.Debug ()

let test_library () =
  Observe_lwt_unix.Test.with_capture test_config (fun capture ->
      Lwt.bind (My_library.run ()) (fun () ->
          let logs = Observe.Capture.logs capture in
          check_logs logs;
          Lwt.return_unit))
```

Concurrent scopes are isolated. Nested capture restores the outer scope.
Success, exception, and `Lwt.Canceled` close the scope and restore its prior
binding. A callback that was registered inside the scope but runs after closure
cannot fall through to production. The binding does not propagate through
`Lwt_preemptive.detach`, OS threads, or another runtime.

Invalid capacity or runtime ownership failure raises
`Observe_lwt_unix.Test.Capture_error` in the returned promise before the test
callback runs.

## Repository layout

- `packages/observe/` contains the core library and PPX.
- `packages/observe/lwt/` and `packages/observe/unix/` contain the separate
  Lwt runtime and Unix platform mechanisms.
- `packages/observe/lwt/unix/` contains the ready composition.
- `packages/observe/doc/` contains the odoc package landing page.
- `test/observe/public/` and `test/observe/interfaces/` exercise the public API
  and compile-time boundary.
- `test/observe/runtime/`, `capture/`, `lwt/`, `unix/`, and `concurrency/`
  exercise routing, lifecycle, real adapter behavior, and synchronization
  contracts; `test/observe/ppx/` exercises the deriver and value extension.
- `test/observe/support/` contains shared test-only adapters and helpers.
- `docs/` indexes repository documentation.
