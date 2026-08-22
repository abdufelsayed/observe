exception Invalid_utf8

let control_escapes =
  [|
    "\\u0000";
    "\\u0001";
    "\\u0002";
    "\\u0003";
    "\\u0004";
    "\\u0005";
    "\\u0006";
    "\\u0007";
    "\\b";
    "\\t";
    "\\n";
    "\\u000b";
    "\\f";
    "\\r";
    "\\u000e";
    "\\u000f";
    "\\u0010";
    "\\u0011";
    "\\u0012";
    "\\u0013";
    "\\u0014";
    "\\u0015";
    "\\u0016";
    "\\u0017";
    "\\u0018";
    "\\u0019";
    "\\u001a";
    "\\u001b";
    "\\u001c";
    "\\u001d";
    "\\u001e";
    "\\u001f";
  |]

let null buffer = Buffer.add_string buffer "null"
let unit buffer () = Buffer.add_string buffer "{}"

let bool buffer value =
  Buffer.add_string buffer (if value then "true" else "false")

(* [caml_format_float] is the OCaml compiler-runtime primitive that backs
   [Printf] float formatting. Binding it directly skips per-call format
   parsing; ["%.16g"] matches the Jsonm encoder (and therefore Repr) byte for
   byte, which keeps direct writers and compatibility projections on one
   numeric contract. The primitive is part of the compiler runtime and changes
   only with an OCaml release. *)
external runtime_format_float : string -> float -> string = "caml_format_float"

let float_to_string value = runtime_format_float "%.16g" value

let float buffer value =
  match classify_float value with
  | FP_nan -> Buffer.add_string buffer "\"nan\""
  | FP_infinite when Float.sign_bit value -> Buffer.add_string buffer "\"-inf\""
  | FP_infinite -> Buffer.add_string buffer "\"inf\""
  | FP_normal | FP_subnormal | FP_zero ->
      Buffer.add_string buffer (float_to_string value)

let rec add_unsigned_int buffer value =
  if value >= 10 then add_unsigned_int buffer (value / 10);
  Buffer.add_char buffer (Char.unsafe_chr (48 + (value mod 10)))

let minimum_int = string_of_int min_int

let decimal_int buffer value =
  if value >= 0 then add_unsigned_int buffer value
  else if value = min_int then Buffer.add_string buffer minimum_int
  else (
    Buffer.add_char buffer '-';
    add_unsigned_int buffer (-value))

let rec add_unsigned_int64 buffer value =
  if Int64.compare value 10L >= 0 then
    add_unsigned_int64 buffer (Int64.div value 10L);
  Buffer.add_char buffer
    (Char.unsafe_chr (48 + Int64.to_int (Int64.rem value 10L)))

let decimal_int64 buffer value =
  if Int64.compare value 0L >= 0 then add_unsigned_int64 buffer value
  else if Int64.equal value Int64.min_int then
    Buffer.add_string buffer "-9223372036854775808"
  else (
    Buffer.add_char buffer '-';
    add_unsigned_int64 buffer (Int64.neg value))

let decimal_int32 buffer value = decimal_int64 buffer (Int64.of_int32 value)
let maximum_exact_integer = 9_007_199_254_740_992L

let int buffer value =
  let value64 = Int64.of_int value in
  if
    Int64.compare value64 (Int64.neg maximum_exact_integer) >= 0
    && Int64.compare value64 maximum_exact_integer <= 0
  then decimal_int buffer value
  else float buffer (float_of_int value)

let int32 = decimal_int32

let int64 buffer value =
  if
    Int64.compare value (Int64.neg maximum_exact_integer) >= 0
    && Int64.compare value maximum_exact_integer <= 0
  then decimal_int64 buffer value
  else float buffer (Int64.to_float value)

let append_string buffer value length first_escape =
  Buffer.add_char buffer '"';
  (if first_escape < 0 then Buffer.add_string buffer value
   else
     let rec escaped start index =
       if index = length then
         Buffer.add_substring buffer value start (length - start)
       else
         let character = String.unsafe_get value index in
         let code = Char.code character in
         if character = '"' || character = '\\' || code < 0x20 then (
           Buffer.add_substring buffer value start (index - start);
           if character = '"' then Buffer.add_string buffer "\\\""
           else if character = '\\' then Buffer.add_string buffer "\\\\"
           else Buffer.add_string buffer control_escapes.(code);
           escaped (index + 1) (index + 1))
         else escaped start (index + 1)
     in
     escaped 0 first_escape);
  Buffer.add_char buffer '"'

let string buffer value =
  let length = String.length value in
  let rec scan index first_escape has_non_ascii =
    if index = length then (first_escape, has_non_ascii)
    else
      let character = String.unsafe_get value index in
      let code = Char.code character in
      let first_escape =
        if
          first_escape < 0
          && (character = '"' || character = '\\' || code < 0x20)
        then index
        else first_escape
      in
      scan (index + 1) first_escape (has_non_ascii || code >= 0x80)
  in
  let first_escape, has_non_ascii = scan 0 (-1) false in
  if has_non_ascii && not (Utf8.is_valid value) then raise Invalid_utf8;
  append_string buffer value length first_escape

let trusted_string buffer value =
  let length = String.length value in
  let rec scan index first_escape =
    if index = length then first_escape
    else
      let character = String.unsafe_get value index in
      let code = Char.code character in
      scan (index + 1)
        (if
           first_escape < 0
           && (character = '"' || character = '\\' || code < 0x20)
         then index
         else first_escape)
  in
  append_string buffer value length (scan 0 (-1))

let char buffer value = string buffer (String.make 1 value)
let bytes buffer value = string buffer (Bytes.unsafe_to_string value)

let name buffer value =
  string buffer value;
  Buffer.add_char buffer ':'

let trusted_name buffer value =
  trusted_string buffer value;
  Buffer.add_char buffer ':'

let array encode buffer values =
  Buffer.add_char buffer '[';
  let length = Array.length values in
  let rec write index =
    if index < length then (
      if index <> 0 then Buffer.add_char buffer ',';
      encode buffer (Array.unsafe_get values index);
      write (index + 1))
  in
  write 0;
  Buffer.add_char buffer ']'

let list encode buffer values =
  Buffer.add_char buffer '[';
  let rec write = function
    | [] -> ()
    | [ value ] -> encode buffer value
    | value :: rest ->
        encode buffer value;
        Buffer.add_char buffer ',';
        write rest
  in
  write values;
  Buffer.add_char buffer ']'

let option encode buffer = function
  | None -> null buffer
  | Some value ->
      Buffer.add_string buffer "{\"some\":";
      encode buffer value;
      Buffer.add_char buffer '}'
