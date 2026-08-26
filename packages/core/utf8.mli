val is_valid : string -> bool

val scalar_count : string -> int
(** Count Unicode scalar values. The input must be valid UTF-8. *)

val byte_offset : string -> characters:int -> int
(** Return the byte offset after [characters] Unicode scalar values. Counts
    beyond the end are clamped to the string length. A negative count raises
    [Invalid_argument]. *)
