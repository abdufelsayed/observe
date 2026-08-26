let byte value index = Char.code (String.unsafe_get value index)

let continuation value length index =
  index < length
  &&
  let byte = byte value index in
  byte >= 0x80 && byte <= 0xbf

(* Return the width of the scalar beginning at [index].  [0] means malformed
   input.  Keeping the width calculation in one place makes the validity,
   counting, and slicing paths agree at every boundary. *)
let scalar_width value length index =
  if index >= length then 0
  else
    let leading = byte value index in
    if leading < 0x80 then 1
    else if leading >= 0xc2 && leading <= 0xdf then
      if continuation value length (index + 1) then 2 else 0
    else if leading = 0xe0 then
      if
        index + 2 < length
        && byte value (index + 1) >= 0xa0
        && byte value (index + 1) <= 0xbf
        && continuation value length (index + 2)
      then 3
      else 0
    else if
      (leading >= 0xe1 && leading <= 0xec)
      || (leading >= 0xee && leading <= 0xef)
    then
      if
        continuation value length (index + 1)
        && continuation value length (index + 2)
      then 3
      else 0
    else if leading = 0xed then
      if
        index + 2 < length
        && byte value (index + 1) >= 0x80
        && byte value (index + 1) <= 0x9f
        && continuation value length (index + 2)
      then 3
      else 0
    else if leading = 0xf0 then
      if
        index + 3 < length
        && byte value (index + 1) >= 0x90
        && byte value (index + 1) <= 0xbf
        && continuation value length (index + 2)
        && continuation value length (index + 3)
      then 4
      else 0
    else if leading >= 0xf1 && leading <= 0xf3 then
      if
        continuation value length (index + 1)
        && continuation value length (index + 2)
        && continuation value length (index + 3)
      then 4
      else 0
    else if leading = 0xf4 then
      if
        index + 3 < length
        && byte value (index + 1) >= 0x80
        && byte value (index + 1) <= 0x8f
        && continuation value length (index + 2)
        && continuation value length (index + 3)
      then 4
      else 0
    else 0

let is_valid value =
  let length = String.length value in
  let index = ref 0 in
  let valid = ref true in
  while !valid && !index < length do
    let width = scalar_width value length !index in
    if width = 0 then valid := false else index := !index + width
  done;
  !valid

let scalar_count value =
  let length = String.length value in
  let index = ref 0 in
  let result = ref 0 in
  while !index < length do
    let width = scalar_width value length !index in
    if width = 0 then invalid_arg "Observe.Utf8.scalar_count: invalid UTF-8"
    else (
      index := !index + width;
      result := !result + 1)
  done;
  !result

let byte_offset value ~characters =
  if characters < 0 then invalid_arg "Observe.Utf8.byte_offset: negative count"
  else
    let length = String.length value in
    let index = ref 0 in
    let remaining = ref characters in
    while !remaining > 0 && !index < length do
      let width = scalar_width value length !index in
      if width = 0 then invalid_arg "Observe.Utf8.byte_offset: invalid UTF-8"
      else (
        index := !index + width;
        remaining := !remaining - 1)
    done;
    !index
