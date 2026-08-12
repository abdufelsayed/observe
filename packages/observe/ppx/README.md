# Observe PPX

`observe.ppx` provides the `[@@deriving observe]` type-description deriver and
the namespaced `[%observe.value ...]` free-form value extension. Generated code
uses the public `Observe.Type` and `Observe.Value` paths; application runtime
code does not link the PPX implementation.

Enable it in Dune:

```lisp
(executable
 (name main)
 (libraries observe)
 (preprocess
  (pps observe.ppx)))
```

## Typed descriptions

The deriver emits an Observe description named after the declared type:

```ocaml
type event = User_login of { user_id : int; method_ : string }
[@@deriving observe]

let description : event Observe.Type.t = event_t

let () =
  Observe.Logs.info
    (Observe.Logs.structured description
       (User_login { user_id = 42; method_ = "oauth" }))
```

The description pairs Repr's machine representation with presentation metadata
generated from the OCaml type. JSON and binary operations still use Repr, while
readable output can distinguish strings, ordinary constructors, and
polymorphic constructors. `Observe.Type.repr` exposes the Repr description
when another library needs it.

The deriver targets `Observe.Type` as its default description library. It uses
the Repr PPX engine for machine descriptions and package-owned generation for
presentation metadata and inline-record variant constructors.

Inline-record lowering currently supports a single, non-parameterized,
non-recursive ordinary variant declaration. Recursive or mutually recursive
groups, type parameters, GADT result types, and existential constructor
variables receive located compile errors. Ordinary declarations without
inline records retain the broader `ppx_repr` feature set.

## Free-form values

`[%observe.value ...]` accepts integer, float, string, and Boolean literals;
record syntax as an object; list literals; and `Some` or `None`. The extension
expands to a `unit -> Observe.Value.t` thunk, so it passes directly to
`Observe.Logs.free` and runs only after admission:

```ocaml
Observe.Logs.info
  (Observe.Logs.free
     [%observe.value
       {
         action = "user_login";
         user_id = 42;
         methods = [ "oauth"; "passkey" ];
         previous_user = None;
       }])
```

Record field names become object keys and must be unqualified identifiers.
Record updates and arbitrary OCaml expressions are rejected rather than being
given a guessed representation.

Embed a dynamic or otherwise unsupported OCaml expression with an explicit
description:

```ocaml
let user_id = 42 in
Observe.Logs.info
  (Observe.Logs.free
     [%observe.value
       {
         action = "user_login";
         user_id =
           [%observe.value.embed (Observe.Type.int, user_id)];
       }])
```

The embedded value is retained by reference and interpreted only when a
formatter projects it. Suffixed numeric literals such as `1L` likewise require
`[%observe.value.embed (description, value)]` with an appropriate description.
