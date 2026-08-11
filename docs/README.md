# Documentation

- [`../README.md`](../README.md) is the user guide: installation, authoring,
  ready Lwt-Unix initialization, authoring, scoped capture, custom runtime
  composition, ownership, and safety boundaries.
- [`../packages/observe/doc/index.mld`](../packages/observe/doc/index.mld) is the
  installed odoc landing page for the `observe` package.
- [`../packages/observe/ppx/README.md`](../packages/observe/ppx/README.md)
  documents `observe.ppx`, `[@@deriving observe]`, and the namespaced
  `[%observe.value ...]` syntax.

The shipped package contains a runtime-neutral logging core, separate Lwt and
Unix mechanism sublibraries, and the ready `Observe_lwt_unix` composition. The
core performs no I/O. Runtime integrations implement `Observe.Runtime.S`;
platform integrations implement `Observe.Platform.S`;
`Observe.Runtime.Make` composes those roles.

Readable terminal presentation is owned by the portable core. The Unix
platform reports standard error's detected color capability and writes the
completed bytes unchanged. One semantic palette is automatically rendered as
truecolor, 256-color, 16-color, or plain output.
