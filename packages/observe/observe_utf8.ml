let is_valid value =
  let length = String.length value in
  let byte index = Char.code value.[index] in
  let is_continuation index =
    index < length
    &&
    let byte = byte index in
    byte >= 0x80 && byte <= 0xbf
  in
  let sequence_length index leading =
    if leading >= 0xc2 && leading <= 0xdf && is_continuation (index + 1) then
      Some 2
    else if leading = 0xe0 && index + 2 < length then
      let second = byte (index + 1) in
      if second >= 0xa0 && second <= 0xbf && is_continuation (index + 2) then
        Some 3
      else None
    else if
      ((leading >= 0xe1 && leading <= 0xec)
      || (leading >= 0xee && leading <= 0xef))
      && is_continuation (index + 1)
      && is_continuation (index + 2)
    then Some 3
    else if leading = 0xed && index + 2 < length then
      let second = byte (index + 1) in
      if second >= 0x80 && second <= 0x9f && is_continuation (index + 2) then
        Some 3
      else None
    else if leading = 0xf0 && index + 3 < length then
      let second = byte (index + 1) in
      if
        second >= 0x90
        && second <= 0xbf
        && is_continuation (index + 2)
        && is_continuation (index + 3)
      then Some 4
      else None
    else if
      leading >= 0xf1
      && leading <= 0xf3
      && is_continuation (index + 1)
      && is_continuation (index + 2)
      && is_continuation (index + 3)
    then Some 4
    else if leading = 0xf4 && index + 3 < length then
      let second = byte (index + 1) in
      if
        second >= 0x80
        && second <= 0x8f
        && is_continuation (index + 2)
        && is_continuation (index + 3)
      then Some 4
      else None
    else None
  in
  let rec check index =
    if index = length then true
    else
      let leading = byte index in
      if leading < 0x80 then check (index + 1)
      else
        match sequence_length index leading with
        | None -> false
        | Some length -> check (index + length)
  in
  check 0
