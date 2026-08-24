type ('record, 'builder) t
type 'record patch
type fragment
type field
type identity
type 'record patch_builder

val record :
  ?name:string ->
  builder:('record patch_builder -> 'builder) ->
  'record Type.t ->
  ('record, 'builder) t
(** Establish a record-root schema. Generated schemas pass a qualified stable
    [name]; manual schemas default to the record description's name. Raises
    [Invalid_argument] when the description is not record-shaped. *)

val field_patch :
  'record patch_builder -> string -> 'a Type.t -> 'a -> 'record patch

val nested_patch :
  'record patch_builder ->
  string ->
  using:('nested, 'nested_builder) t ->
  'nested patch ->
  'record patch

val combine : 'record patch_builder -> 'record patch list -> 'record patch
val name : ('record, 'builder) t -> string
val identity : ('record, 'builder) t -> identity
val same_identity : identity -> identity -> bool
val builder : ('record, 'builder) t -> 'builder

val freeze_complete :
  ('record, 'builder) t ->
  'record ->
  Snapshot.context ->
  depth:int ->
  (Snapshot.value, Snapshot.error) result

val fragment : 'a Type.t -> 'a -> fragment
val fragment_of_value : Value.t -> fragment

val materialize :
  fragment ->
  Snapshot.context ->
  depth:int ->
  (Snapshot.value, Snapshot.error) result

val patch_fragment : 'record patch -> fragment
val field : string -> fragment -> field
val make_patch : ('record, 'builder) t -> field option list -> 'record patch
val make_patch_fields : ('record, 'builder) t -> field list -> 'record patch

val make_identified_patch :
  'record patch_builder -> field option list -> 'record patch

val make_identified_patch_fields :
  'record patch_builder -> field list -> 'record patch

val make_identified_error_patch :
  'record patch_builder -> fragment -> 'record patch

val combine_identified_patches :
  'record patch_builder -> 'record patch list -> 'record patch

val has_error : 'record patch -> bool
val body : ('record, 'builder) t -> 'record patch -> fragment
