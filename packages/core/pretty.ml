type error = Invalid_utf8 | Non_finite_float | Unsupported_value | Malformed

exception Error of error

type style = Formatter_intf.style = Plain | Ansi_16 | Ansi_256 | Truecolor
type severity = Debug | Info | Warn | Error
type color = { ansi_16 : string; ansi_256 : string; truecolor : string }

let metadata =
  { ansi_16 = "90"; ansi_256 = "38;5;244"; truecolor = "38;2;111;119;130" }

let debug =
  { ansi_16 = "90"; ansi_256 = "38;5;246"; truecolor = "38;2;139;149;167" }

let info =
  { ansi_16 = "96"; ansi_256 = "38;5;39"; truecolor = "38;2;14;165;233" }

let warn =
  { ansi_16 = "93"; ansi_256 = "38;5;178"; truecolor = "38;2;217;149;0" }

let error =
  { ansi_16 = "91"; ansi_256 = "38;5;203"; truecolor = "38;2;240;82;82" }

let constructor_color =
  { ansi_16 = "95"; ansi_256 = "38;5;170"; truecolor = "38;2;208;107;223" }

let field_color =
  { ansi_16 = "95"; ansi_256 = "38;5;141"; truecolor = "38;2;167;139;250" }

let string_color =
  { ansi_16 = "92"; ansi_256 = "38;5;78"; truecolor = "38;2;66;184;131" }

let number_color =
  { ansi_16 = "93"; ansi_256 = "38;5;214"; truecolor = "38;2;245;158;11" }

let boolean_color =
  { ansi_16 = "95"; ansi_256 = "38;5;213"; truecolor = "38;2;232;121;249" }

type t = {
  buffer : Buffer.t;
  style : style;
  mutable path : bool array;
  mutable depth : int;
}

type placement =
  | Inline
  | Root
  | Field of { last : bool; name : string }
  | Constructor of { last : bool; name : string }
  | Index of { last : bool; index : int }

let create ~capacity style =
  {
    buffer = Buffer.create capacity;
    style;
    path = Array.make 8 false;
    depth = 0;
  }

let contents renderer = Buffer.contents renderer.buffer
let space renderer = Buffer.add_char renderer.buffer ' '
let newline renderer = Buffer.add_char renderer.buffer '\n'

let start_style ?(bold = false) renderer color =
  match renderer.style with
  | Plain -> ()
  | Ansi_16 | Ansi_256 | Truecolor ->
      let code =
        match renderer.style with
        | Ansi_16 -> color.ansi_16
        | Ansi_256 -> color.ansi_256
        | Truecolor -> color.truecolor
        | Plain -> assert false
      in
      Buffer.add_string renderer.buffer "\027[";
      if bold then Buffer.add_string renderer.buffer "1;";
      Buffer.add_string renderer.buffer code;
      Buffer.add_char renderer.buffer 'm'

let end_style renderer =
  match renderer.style with
  | Plain -> ()
  | Ansi_16 | Ansi_256 | Truecolor ->
      Buffer.add_string renderer.buffer "\027[0m"

let styled ?bold renderer color value =
  start_style ?bold renderer color;
  Buffer.add_string renderer.buffer value;
  end_style renderer

let level_color = function
  | Debug -> debug
  | Info -> info
  | Warn -> warn
  | Error -> error

let level_label = function
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"

let valid_string value =
  if Utf8.is_valid value then value else raise (Error Invalid_utf8)

let text renderer value =
  let length = String.length value in
  let rec scan index first_escape has_non_ascii =
    if index = length then (first_escape, has_non_ascii)
    else
      let code = Char.code (String.unsafe_get value index) in
      scan (index + 1)
        (if first_escape < 0 && (code < 0x20 || code = 0x7f) then index
         else first_escape)
        (has_non_ascii || code >= 0x80)
  in
  let first_escape, has_non_ascii = scan 0 (-1) false in
  if has_non_ascii && not (Utf8.is_valid value) then raise (Error Invalid_utf8);
  if first_escape < 0 then Buffer.add_string renderer.buffer value
  else
    let hex = "0123456789abcdef" in
    let rec escaped start index =
      if index = length then
        Buffer.add_substring renderer.buffer value start (length - start)
      else
        let character = String.unsafe_get value index in
        let code = Char.code character in
        if code < 0x20 || code = 0x7f then (
          Buffer.add_substring renderer.buffer value start (index - start);
          (match character with
          | '\b' -> Buffer.add_string renderer.buffer "\\b"
          | '\012' -> Buffer.add_string renderer.buffer "\\f"
          | '\n' -> Buffer.add_string renderer.buffer "\\n"
          | '\r' -> Buffer.add_string renderer.buffer "\\r"
          | '\t' -> Buffer.add_string renderer.buffer "\\t"
          | _ ->
              Buffer.add_string renderer.buffer "\\u00";
              Buffer.add_char renderer.buffer hex.[code lsr 4];
              Buffer.add_char renderer.buffer hex.[code land 0xf]);
          escaped (index + 1) (index + 1))
        else escaped start (index + 1)
    in
    escaped 0 first_escape

let quoted renderer value =
  try Json_writer.string renderer.buffer value
  with Json_writer.Invalid_utf8 -> raise (Error Invalid_utf8)

let nanoseconds_per_day = 86_400_000_000_000L

let add_two_digits buffer value =
  Buffer.add_char buffer (Char.unsafe_chr (48 + (value / 10)));
  Buffer.add_char buffer (Char.unsafe_chr (48 + (value mod 10)))

let add_three_digits buffer value =
  Buffer.add_char buffer (Char.unsafe_chr (48 + (value / 100)));
  Buffer.add_char buffer (Char.unsafe_chr (48 + (value / 10 mod 10)));
  Buffer.add_char buffer (Char.unsafe_chr (48 + (value mod 10)))

let timestamp renderer unix_ns =
  let within_day = Int64.rem unix_ns nanoseconds_per_day in
  let within_day =
    if Int64.compare within_day 0L < 0 then
      Int64.add within_day nanoseconds_per_day
    else within_day
  in
  let milliseconds = Int64.div within_day 1_000_000L in
  let hours = Int64.to_int (Int64.div milliseconds 3_600_000L) in
  let minutes = Int64.to_int (Int64.rem (Int64.div milliseconds 60_000L) 60L) in
  let seconds = Int64.to_int (Int64.rem (Int64.div milliseconds 1_000L) 60L) in
  let milliseconds = Int64.to_int (Int64.rem milliseconds 1_000L) in
  start_style renderer metadata;
  add_two_digits renderer.buffer hours;
  Buffer.add_char renderer.buffer ':';
  add_two_digits renderer.buffer minutes;
  Buffer.add_char renderer.buffer ':';
  add_two_digits renderer.buffer seconds;
  Buffer.add_char renderer.buffer '.';
  add_three_digits renderer.buffer milliseconds;
  end_style renderer

let header renderer ~unix_ns ~severity ~scope =
  timestamp renderer unix_ns;
  space renderer;
  styled ~bold:true renderer (level_color severity) (level_label severity);
  space renderer;
  start_style ~bold:true renderer (level_color severity);
  Buffer.add_char renderer.buffer '[';
  text renderer scope;
  Buffer.add_char renderer.buffer ']';
  end_style renderer

let ensure_path renderer =
  if renderer.depth = Array.length renderer.path then (
    let path = Array.make (max 1 (renderer.depth * 2)) false in
    Array.blit renderer.path 0 path 0 renderer.depth;
    renderer.path <- path)

let push_parent renderer ~last =
  ensure_path renderer;
  Array.unsafe_set renderer.path renderer.depth last;
  renderer.depth <- renderer.depth + 1

let prefix renderer =
  Buffer.add_string renderer.buffer "  ";
  let rec add index =
    if index < renderer.depth then (
      Buffer.add_string renderer.buffer
        (if Array.unsafe_get renderer.path index then "   " else "│  ");
      add (index + 1))
  in
  add 0

let item renderer ~last ~label ~scalar =
  prefix renderer;
  styled renderer metadata (if last then "└─" else "├─");
  space renderer;
  (match label with
  | `None -> ()
  | `Field name ->
      start_style renderer field_color;
      text renderer name;
      end_style renderer;
      if scalar then (
        styled renderer metadata ":";
        space renderer)
  | `Constructor name ->
      start_style ~bold:true renderer constructor_color;
      text renderer name;
      end_style renderer;
      if scalar then (
        styled renderer metadata ":";
        space renderer)
  | `Index index ->
      start_style renderer field_color;
      Buffer.add_char renderer.buffer '[';
      Json_writer.decimal_int renderer.buffer index;
      Buffer.add_char renderer.buffer ']';
      end_style renderer;
      if scalar then (
        styled renderer metadata ":";
        space renderer));
  if scalar then false
  else (
    newline renderer;
    push_parent renderer ~last;
    true)

let place renderer placement ~scalar =
  match placement with
  | Inline -> false
  | Root ->
      if scalar then item renderer ~last:true ~label:`None ~scalar:true
      else false
  | Field { last; name } -> item renderer ~last ~label:(`Field name) ~scalar
  | Constructor { last; name } ->
      item renderer ~last ~label:(`Constructor name) ~scalar
  | Index { last; index } -> item renderer ~last ~label:(`Index index) ~scalar

let finish renderer nested = if nested then renderer.depth <- renderer.depth - 1

let field renderer ~last ~name ~scalar =
  item renderer ~last ~label:(`Field name) ~scalar

let index renderer ~last ~index ~scalar =
  item renderer ~last ~label:(`Index index) ~scalar

let constructor renderer ~last ~name ~scalar =
  item renderer ~last ~label:(`Constructor name) ~scalar

let null renderer = styled renderer metadata "null"

let bool renderer value =
  styled renderer boolean_color (if value then "true" else "false")

let int renderer value =
  start_style renderer number_color;
  Json_writer.decimal_int renderer.buffer value;
  end_style renderer

let int32 renderer value =
  start_style renderer number_color;
  Json_writer.decimal_int32 renderer.buffer value;
  end_style renderer

let int64 renderer value =
  start_style renderer number_color;
  Json_writer.decimal_int64 renderer.buffer value;
  end_style renderer

let number renderer value = styled renderer number_color value

let float renderer value =
  match classify_float value with
  | FP_nan | FP_infinite -> raise (Error Non_finite_float)
  | FP_normal | FP_subnormal | FP_zero ->
      number renderer (Json_writer.float_to_string value)

let string renderer value =
  start_style renderer string_color;
  quoted renderer value;
  end_style renderer

let empty_record renderer = styled renderer metadata "{}"
let empty_list renderer = Buffer.add_string renderer.buffer "[]"

let variant renderer ~polymorphic name =
  start_style ~bold:true renderer constructor_color;
  if polymorphic then Buffer.add_char renderer.buffer '`';
  text renderer name;
  end_style renderer

let list_start renderer = Buffer.add_char renderer.buffer '['
let list_separator renderer = Buffer.add_string renderer.buffer ", "
let list_end renderer = Buffer.add_char renderer.buffer ']'

type rendered = Scalar of (t -> unit) | Node of (t -> placement -> unit)

let render renderer placement = function
  | Scalar append ->
      let nested = place renderer placement ~scalar:true in
      append renderer;
      finish renderer nested
  | Node append -> append renderer placement
