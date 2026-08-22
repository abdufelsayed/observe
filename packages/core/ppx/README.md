# Observe PPX

`observe.ppx` provides admission-preserving logging extensions, the
`[@@deriving observe]` type-description deriver, and the namespaced
`[%observe.value ...]` untyped-value extension. Generated code uses
`Observe.Logs`, `Observe.Type`, `Observe.Value`, and the documented
`Observe.Generated_runtime` compatibility ABI; application runtime code does
not link the PPX implementation.

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
  typed sync_failed_schema { source = "postgres"; attempts = 3 }]
```

They expand semantically to the ordinary public API:

```ocaml
Observe.Logs.info (fun m ->
  m.text ~tag:"auth" "user %d logged in" user_id)

Observe.Logs.warn (fun m ->
  m.value
    (Observe.Value.object_
       [
         ("action", Observe.Value.string "retry");
         ("attempt", Observe.Value.int 2);
       ]))

Observe.Logs.error (fun m ->
  m.typed sync_failed_schema { source = "postgres"; attempts = 3 })
```

The callback remains the only deferred authoring boundary. The active route and
configured level are checked before the generated callback runs, so formatted
arguments, untyped objects, and typed values preserve the manual API's admission
behavior. Ordinary callback exception containment is unchanged.

The fixed-level forms are `[%observe.debug ...]`, `[%observe.info ...]`,
`[%observe.warn ...]`, and `[%observe.error ...]`. Each accepts exactly one of
`text`, `untyped`, or `typed`. Anonymous record syntax preserves E1's
annotation-free primitive literals, nested objects, lists, and options without
nesting `[%observe.value]`. An arbitrary expression uses an explicit
description such as `string value` or `list string values`. A dynamic level
uses a pair:

```ocaml
[%observe.emit (level, typed event_schema event)]
```

which expands to:

```ocaml
Observe.Logs.log ~level (fun m -> m.typed event_schema event)
```

The restricted body vocabulary is deliberate: misspelled or unsupported forms
receive a located PPX error instead of expanding into an unrelated expression.
The manual `Observe.Logs` API remains available and requires no PPX.

### Wide contributions

`[%observe.set]` keeps open and schema-locked wide authoring at one PPX layer:

```ocaml
let open_log = Observe.Logs.create ~name:"checkout" () in
[%observe.set open_log
  untyped
    {
      cart_id = string cart_id;
      phase = "started";
      customer = { id = string customer_id; plan = string plan };
    }];

let typed_log =
  Observe.Logs.create_typed ~name:"checkout" checkout_event_schema
in
[%observe.set typed_log
  { phase = Authorized authorization_id; attempts = attempts + 1 }];
[%observe.set typed_log error Observe.Error.exn exn]
```

Self-describing anonymous syntax expands through the same package-owned value
conversion as E1 and becomes an untyped patch through the PPX runtime contract.
Explicitly described arbitrary expressions retain their supplied description.
Typed sparse records expand through the generated builder attached to the
handle, so OCaml checks field names, nested patch shapes, and value types
without PPX-inserted annotations. The extension does not infer a schema from
syntax.

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
| `Observe.Type.of_repr` | Repr compatibility only; no implicit bounded freezer | Opaque Repr operations outside completed logs |

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
description compile the same direct JSON and pretty dispatch. The deriver
supplies equivalent projectors automatically. Omitting them preserves
compatibility with existing Repr-style description code, which then uses the
generic compatibility path.

Recursive declarations use Repr's recursive machine description and generated
recursive direct projections. Logging checks depth before descending into each
described value, so cyclic values terminate at the canonical limit and the
whole observation is withheld. This does not make arbitrary direct Repr
operations cycle-safe.

### Repr interoperability

`Observe.Type.repr description` exposes the underlying Repr description for
decoding, equality, comparison, binary operations, schemas or another Repr
consumer. `Observe.Type.of_repr repr` lifts an existing description for direct
interoperability. Because Repr is abstract, that lift cannot supply Observe's
bounded freezer. Such a value is withheld from completed logs rather than
falling back to unbounded traversal or retaining a live value.

If an integration already has a direct compact JSON writer, it can retain the
opaque Repr description without paying for the generic encoder:

```ocaml
let description = Observe.Type.of_repr ~json:append_json existing_repr
```

The writer appends exactly one compact value to the supplied buffer and must
use the same representation accepted by `existing_repr`'s decoder. It improves
direct JSON compatibility; it does not establish canonical logging safety.

`Observe.Type.to_json_string description value` is the allocating direct
convenience API. Logging instead invokes the bounded freezer once and every
formatter consumes the resulting immutable snapshot.

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
value-construction surface; it is not nested inside a logging extension. To log
one lazily, use the manual callback's explicit `m.value` path:

```ocaml
Observe.Logs.info (fun m ->
  m.value
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
Observe.Logs.info (fun m ->
  m.value
    [%observe.value
      {
        action = "user_login";
        user_id =
          [%observe.value.embed (Observe.Type.int, user_id)];
      }])
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
