let byte value index = Char.code (String.unsafe_get value index)

let continuation value length index =
  if index >= length then false
  else
    let byte = byte value index in
    byte >= 0x80 && byte <= 0xbf

let rec check value length index =
  if index = length then true
  else
    let leading = byte value index in
    if leading < 0x80 then check value length (index + 1)
    else if leading >= 0xc2 && leading <= 0xdf then
      continuation value length (index + 1) && check value length (index + 2)
    else if leading = 0xe0 && index + 2 < length then
      let second = byte value (index + 1) in
      second >= 0xa0
      && second <= 0xbf
      && continuation value length (index + 2)
      && check value length (index + 3)
    else if
      (leading >= 0xe1 && leading <= 0xec)
      || (leading >= 0xee && leading <= 0xef)
    then
      continuation value length (index + 1)
      && continuation value length (index + 2)
      && check value length (index + 3)
    else if leading = 0xed && index + 2 < length then
      let second = byte value (index + 1) in
      second >= 0x80
      && second <= 0x9f
      && continuation value length (index + 2)
      && check value length (index + 3)
    else if leading = 0xf0 && index + 3 < length then
      let second = byte value (index + 1) in
      second >= 0x90
      && second <= 0xbf
      && continuation value length (index + 2)
      && continuation value length (index + 3)
      && check value length (index + 4)
    else if leading >= 0xf1 && leading <= 0xf3 then
      continuation value length (index + 1)
      && continuation value length (index + 2)
      && continuation value length (index + 3)
      && check value length (index + 4)
    else if leading = 0xf4 && index + 3 < length then
      let second = byte value (index + 1) in
      second >= 0x80
      && second <= 0x8f
      && continuation value length (index + 2)
      && continuation value length (index + 3)
      && check value length (index + 4)
    else false

let is_valid value = check value (String.length value) 0
