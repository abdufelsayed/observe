# Documentation

- [`../README.md`](../README.md) is the user guide: installation, authoring,
  configuration, direct-runtime capture, ownership, and safety boundaries.
- [`../packages/observe/doc/index.mld`](../packages/observe/doc/index.mld) is the
  installed odoc landing page for the `observe` package.
- [`../packages/observe/ppx/README.md`](../packages/observe/ppx/README.md)
  documents `observe.ppx`, `[@@deriving observe]`, and the namespaced
  `[%observe.value ...]` syntax.

The shipped package is a runtime-neutral logging core. It performs no I/O and
does not currently provide wide-event or metric APIs. Runtime integrations
implement `Observe.Runtime.S`; platform integrations implement
`Observe.Platform.S`; `Observe.Runtime.Make` composes those roles.

