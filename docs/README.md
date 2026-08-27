# Documentation ownership

- [`../README.md`](../README.md) is the short landing page and first successful
  path.
- [`../packages/core/doc`](../packages/core/doc) contains the installed human
  manual: getting started, logging, production policy, outputs, testing, and
  package boundaries.
- [`../packages/core/ppx/README.md`](../packages/core/ppx/README.md) is the full
  generated-syntax reference.
- Each remaining [`../packages`](../packages) documentation directory owns the
  package-specific API landing page for its effect boundary.
- [`../test/README.md`](../test/README.md) and
  [`../bench/README.md`](../bench/README.md) are maintainer guides.

The six opam packages are `observe`, `observe-lwt`, `observe-lwt-unix`,
`observe-fs`, `observe-fs-lwt`, and `observe-fs-lwt-unix`. `observe.ppx` ships
as a sublibrary of the core package. These divisions express dependencies and
effect ownership, not different classes of user.

The documentation workflow publishes the odoc manual and API reference. Code
that represents a complete user journey also lives in `examples` so CI compiles
it instead of trusting prose snippets alone.
