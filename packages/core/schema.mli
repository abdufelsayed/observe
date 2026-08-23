type ('record, 'builder) t
type 'record patch
type fragment
type field
type identity

val record :
  ?name:string ->
  builder:(identity -> 'builder) ->
  'record Type.t ->
  ('record, 'builder) t
(** Establish a record-root schema. Generated schemas pass a qualified stable
    [name]; manual schemas default to the record description's name. Raises
    [Invalid_argument] when the description is not record-shaped. *)

val name : ('record, 'builder) t -> string
val identity : ('record, 'builder) t -> identity
val same_identity : identity -> identity -> bool
val builder : ('record, 'builder) t -> 'builder

val freeze_complete :
  ('record, 'builder) t -> 'record -> (Snapshot.fragment, Snapshot.error) result

val fragment : 'a Type.t -> 'a -> fragment
val fragment_of_result : (Snapshot.fragment, Snapshot.error) result -> fragment
val patch_fragment : 'record patch -> fragment
val field : string -> fragment -> field
val make_patch : ('record, 'builder) t -> field option list -> 'record patch
val make_patch_fields : ('record, 'builder) t -> field list -> 'record patch
val make_identified_patch : identity -> field option list -> 'record patch
val make_identified_patch_fields : identity -> field list -> 'record patch
val make_identified_error_patch : identity -> fragment -> 'record patch
val combine_identified_patches : identity -> 'record patch list -> 'record patch
val has_error : 'record patch -> bool

val body :
  ('record, 'builder) t ->
  'record patch ->
  (Snapshot.fragment, Snapshot.error) result
