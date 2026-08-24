# Observe PPX

`observe.ppx` provides admission-preserving logging extensions, the
`[@@deriving observe]` type-description deriver, and the namespaced
`[%observe.value ...]` untyped-value extension. Generated code is coordinated
with Observe's runtime support; application code does not call that support
directly or link the PPX implementation.

Enable it in Dune:

```lisp
(executable
 (name main)
 (libraries observe)
 (preprocess
  (pps observe.ppx)))
```

## Logging

The logging extensions remove the callback-builder boilerplate without adding
a second logging model:

```ocaml
[%observe.info text ~tag:"auth" "user %d logged in" user_id]

[%observe.warn
  untyped { action = "retry"; attempt = 2 }]

[%observe.error
  typed ~using:sync_failed_schema { source = "postgres"; attempts = 3 }]

[%observe.error
  error ~using:Observe.Error.exn ~backtrace exn]
```

They expand semantically to the ordinary public API:

```ocaml
Observe.Logs.info (fun m ->
  m.text ~tag:"auth" "user %d logged in" user_id)

Observe.Logs.warn (fun m ->
  let open Observe.Logs in
  m.untyped
  |+ m.field "action" Observe.Type.string "retry"
  |+ m.field "attempt" Observe.Type.int 2
  |> m.seal)

Observe.Logs.error (fun m ->
  m.typed ~using:sync_failed_schema
    { source = "postgres"; attempts = 3 })

Observe.Logs.error (fun m ->
  m.error ~using:Observe.Error.exn ~backtrace exn)
```

The callback remains the only deferred authoring boundary. The active route and
configured level are checked before the generated callback runs, so formatted
arguments, untyped objects, and typed values preserve the manual API's admission
behavior. Ordinary callback exception containment is unchanged.

The fixed-level forms are `[%observe.debug ...]`, `[%observe.info ...]`,
`[%observe.warn ...]`, and `[%observe.error ...]`. Each accepts exactly one of
`text`, `untyped`, `typed`, or `error`. Anonymous record syntax preserves E1's
annotation-free primitive literals, nested objects, lists, and options without
nesting `[%observe.value]`. An arbitrary expression uses an explicit
description such as `string value` or `list string values`. A dynamic level
uses the manual API:

```ocaml
Observe.Logs.log ~level (fun m ->
  m.typed ~using:event_schema event)
```

The restricted event vocabulary is deliberate: misspelled or unsupported forms
receive a located PPX error instead of expanding into an unrelated expression.
The manual `Observe.Logs` API remains available and requires no PPX.

### Wide contributions

`[%observe.set]` keeps open and schema-locked wide authoring at one PPX layer:

```ocaml
let open_log = Observe.Logs.create ~name:"checkout" () in
[%observe.set open_log
  {
    cart_id = Observe.Type.string cart_id;
    phase = "started";
    customer =
      {
        id = Observe.Type.string customer_id;
        plan = Observe.Type.string plan;
      };
  }];
[%observe.info open_log "customer context collected"];

let typed_log =
  Observe.Logs.create_typed ~name:"checkout"
    ~using:checkout_event_schema ()
in
[%observe.set typed_log
  typed { phase = Authorized authorization_id; attempts = attempts + 1 }];
[%observe.set typed_log error ~using:Observe.Error.exn exn]
```

Self-describing anonymous syntax expands through the same package-owned value
conversion as E1 and becomes an untyped patch through the PPX runtime contract.
Explicitly described arbitrary expressions retain their supplied description.
Typed sparse records expand through the generated builder attached to the
handle, so OCaml checks field names, nested patch shapes, and value types
without PPX-inserted annotations. An open record needs no mode marker because
it is the ordinary `set` form. The `typed` marker selects generated sparse
patch construction, which OCaml cannot infer from an arbitrary handle
expression; `error` selects schema-independent error contribution.

A level extension whose first expression is a wide handle appends one explicit
annotation instead of emitting a separate point log:

```ocaml
[%observe.warn checkout "payment provider is retrying"]
```

This is the concise form of
`Observe.Logs.annotate checkout ~level:Observe.Level.Warn (fun () -> ...)`.
The message remains lazy, appears under `logs` in the final wide event, and can
raise the derived operation level. Ordinary point logs remain independent and
are never copied into that list.

## Typed descriptions

The deriver emits an Observe description named after every declared type. For
a record it additionally emits a schema witness, an abstract sparse patch, a
patch constructor, and the builder used by one-layer wide-log PPX authoring.
For a module-local `type t`, the constructor remains the idiomatic `patch`,
while its generated support types are named `t_patch`, `t_patch_author`, and
`t_patch_builder` so they do not claim the consumer's general `patch` type
name.

For example, deriving this record:

```ocaml
type user = { id : int; name : string; roles : string list }
[@@deriving observe]
```

defines `user_t : user Observe.Type.t`, `user_schema`, and
`user_patch ?id ?name ?roles ()`. Its complete-description equivalent is:

```ocaml
let user_t =
  let open Observe.Type in
  record "user" (fun id name roles -> { id; name; roles })
  |+ field "id" int (fun user -> user.id)
  |+ field "name" string (fun user -> user.name)
  |+ field "roles" (list string) (fun user -> user.roles)
  |> sealr
```

Both descriptions retain the Repr representation and support the same public
`Observe.Type` operations. Deriving additionally generates type-specialized
JSON and pretty functions plus a bounded direct freezer. Logging uses the
freezer to create one immutable format-neutral snapshot; capture and official
formatters never retain or revisit the complete OCaml value. Manual combinators
compose equivalent bounded freezer behavior from their component descriptions.

The three paths are therefore one API with different amounts of information:

| Description source | Projection implementation | Intended use |
| --- | --- | --- |
| `[@@deriving observe]` | Generated bounded freezer plus specialized direct projections | Normal types and record schemas |
| `Observe.Type` combinators | Composed bounded freezer and direct projections | Hand-written descriptions |

The generated functions do not eagerly serialize a value. For an admitted
point or active wide contribution, the bounded freezer projects directly into
package-owned immutable nodes. Completion rechecks the universal aggregate
budget before publication. JSON and pretty formatters then consume only that
snapshot.

### Variants

An ordinary variant:

```ocaml
type access = Granted | Denied of string [@@deriving observe]
```

corresponds to:

```ocaml
let access_t =
  let open Observe.Type in
  variant "access" (fun granted denied -> function
    | Granted -> granted
    | Denied reason -> denied reason)
  |~ case0 ~is:(function Granted -> true | Denied _ -> false)
       "Granted" Granted
  |~ case1
       ~project:(function Denied reason -> Some reason | Granted -> None)
       "Denied" string (fun reason -> Denied reason)
  |> sealv
```

A nullary constructor encodes as a JSON string. A constructor with a payload
encodes as a one-field object, following Repr's representation. The generated
specialization matches the constructor directly and calls the specialized
description of its payload.

An inline-record constructor is ordinary OCaml authoring syntax:

```ocaml
type event = User_login of { user_id : int; method_ : string }
[@@deriving observe]

let description : event Observe.Type.t = event_t

let () =
  (* Variants remain valid nested descriptions. Structured point and
     schema-locked wide roots themselves are records. *)
  ignore description
```

The corresponding manual description uses a named payload type because OCaml
does not let separate code name an inline-record constructor's anonymous type:

```ocaml
type user_login = { user_id : int; method_ : string }
type event = User_login of user_login

let user_login_t =
  let open Observe.Type in
  record "user_login" (fun user_id method_ -> { user_id; method_ })
  |+ field "user_id" int (fun event -> event.user_id)
  |+ field "method_" string (fun event -> event.method_)
  |> sealr

let event_t =
  let open Observe.Type in
  variant "event" (fun user_login -> function
    | User_login payload -> user_login payload)
  |~ case1
       ~project:(function User_login payload -> Some payload)
       "User_login" user_login_t (fun payload -> User_login payload)
  |> sealv
```

The PPX preserves the original inline-record type and internally generates the
same payload description plus direct field projection.

Closed polymorphic variants are also supported. Nullary rows are JSON strings;
payload rows are one-field objects. Their pretty presentation preserves the
leading backtick even though JSON intentionally does not.

### Containers, tuples, parameters, and recursion

The generated JSON function specializes primitive fields, records, ordinary
and polymorphic variants, lists, arrays, options, references, lazy values, and
two-to-four element tuples. Other supported Repr shapes call the JSON function
already attached to their expanded `Observe.Type.t` description.

Manual descriptions use the corresponding public combinators directly:

```ocaml
let names_t = Observe.Type.list Observe.Type.string
let lookup_t = Observe.Type.option (Observe.Type.pair Observe.Type.int names_t)
```

Inside Repr records, `None` fields and empty list fields are omitted. Outside a
record, an option uses Repr's boxed JSON representation. Generated and manual
descriptions preserve those rules.

Type parameters become description parameters:

```ocaml
type 'a box = Box of 'a [@@deriving observe]

let string_box_t = box_t Observe.Type.string
```

A parameterized record's sparse patch constructor receives the exact schema
witness so the description and patch identity cannot drift apart:

```ocaml
type 'a envelope = { value : 'a } [@@deriving observe]

let using = envelope_schema Observe.Type.int
let patch = envelope_patch ~using ~value:42 ()
```

The manual form is a function receiving the same component description:

```ocaml
let box_t item_t =
  let open Observe.Type in
  variant "box" (fun box -> function Box value -> box value)
  |~ case1 ~project:(function Box value -> Some value)
       "Box" item_t (fun value -> Box value)
  |> sealv
```

The Repr-compatible constructor and central deconstructor build the machine
description. The `~is` and `~project` functions let the manual Observe
description compile the same direct JSON, pretty, and bounded snapshot
dispatch. The deriver supplies equivalent selectors automatically. Manual
variants require them because Repr does not expose a safe structural traversal
that Observe could use to construct its immutable snapshot.

Recursive declarations use Repr's recursive machine description and generated
recursive direct projections. Logging checks depth before descending into each
described value, so cyclic values terminate at the canonical limit and the
whole observation is withheld. This does not make arbitrary direct Repr
operations cycle-safe.

### Repr interoperability

`Observe.Type.repr description` exposes the underlying Repr description for
decoding, equality, comparison, binary operations, schemas or another Repr
consumer. This direction is always safe: an `Observe.Type.t` contains both the
Repr machine and Observe's bounded semantic projection.

The reverse direction is deliberately absent. A raw `Repr.t` does not expose
enough structure to prove bounded snapshot construction. Use the corresponding
`Observe.Type` combinators, deriving, or `Observe.Type.map` from an existing
Observe description when a value needs to enter logging.

The deriver targets `Observe.Type` as its default description library. It uses
the Repr PPX engine for machine descriptions and package-owned generation for
variant projectors, specialized JSON and pretty functions, and inline-record
variant constructors.

Inline-record lowering supports one ordinary variant declaration, including
type parameters. Self-recursive inline-record variants, mutually recursive
groups containing inline records, GADT result types, and existential
constructor variables receive located compile errors. Ordinary declarations
without inline records retain the broader `ppx_repr` feature set.

## Untyped values

`[%observe.value ...]` accepts integer, float, string, and Boolean literals;
record syntax as an object; list literals; and `Some` or `None`. The extension
expands to an `Observe.Value.t`. It is the explicit compatibility and standalone
value-construction surface; it is not nested inside a logging extension and is
not a logging root. Log the equivalent object through the logging DSL:

```ocaml
[%observe.info
  untyped
    {
      action = "user_login";
      user_id = 42;
      methods = [ "oauth"; "passkey" ];
      previous_user = None;
    }]
```

Record field names become object keys and must be unqualified identifiers.
Record updates and arbitrary OCaml expressions are rejected rather than being
given a guessed representation.

Embed a dynamic or otherwise unsupported OCaml expression with an explicit
description:

```ocaml
let user_id = 42 in
[%observe.info
  untyped
    {
      action = "user_login";
      user_id = Observe.Type.int user_id;
    }]
```

The authoring `Observe.Value.t` temporarily retains the embedded value. An
admitted log freezes it before publication, and no formatter or drain receives
the reference. Suffixed numeric literals such as `1L` likewise require
`[%observe.value.embed (description, value)]` with an appropriate description.

The first untyped example expands semantically to ordinary public values:

```ocaml
Observe.Value.object_
  [
    ("action", Observe.Value.string "user_login");
    ("user_id", Observe.Value.int 42);
    ( "methods",
      Observe.Value.list
        [ Observe.Value.string "oauth"; Observe.Value.string "passkey" ] );
    ("previous_user", Observe.Value.option None);
  ]
```

`Some value` recursively wraps the generated value with
`Observe.Value.option`; an embedded expression becomes
`Observe.Value.embed description value`. The logging callback—not the value
extension—is the deferred boundary shared by text, untyped, and typed
authoring.
