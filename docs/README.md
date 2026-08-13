# Documentation

- [`../README.md`](../README.md) is the user guide: installation, authoring,
  ready Lwt-Unix initialization, authoring, scoped capture, custom I/O
  composition, ownership, and safety boundaries.
- [`../packages/core/doc/index.mld`](../packages/core/doc/index.mld) is the
  installed odoc landing page for the `observe` package.
- [`../packages/core/ppx/README.md`](../packages/core/ppx/README.md)
  documents `observe.ppx`, concise logging extensions,
  `[@@deriving observe]`, and the namespaced `[%observe.value ...]` syntax.
- [`../packages/lwt/doc/index.mld`](../packages/lwt/doc/index.mld) and
  [`../packages/lwt-unix/doc/index.mld`](../packages/lwt-unix/doc/index.mld)
  own the configurable-Lwt and ready-composition package pages.

The repository ships a runtime-neutral `observe` package, configurable
`observe-lwt` I/O, and the ready `observe-lwt-unix` composition package. The
core performs no I/O. Integrations implement one completed `Observe.IO.S`;
`Observe.Make (IO)` owns observer behavior over that implementation.

Pretty console presentation is owned by the portable core. The ready
Lwt-Unix composition reports standard error's detected terminal capability and
writes the completed bytes unchanged. One semantic palette is automatically
rendered as truecolor, 256-color, 16-color, or plain output.
