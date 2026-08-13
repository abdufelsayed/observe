type error = Formatter_intf.error =
  | Invalid_utf8
  | Non_finite_float
  | Unsupported_value
  | Failed

type style = Formatter_intf.style = Plain | Ansi_16 | Ansi_256 | Truecolor
type t = Log.t -> (string, error) result

let create formatter = formatter
let format formatter log = formatter log

let pretty_severity = function
  | Level.Debug -> Pretty.Debug
  | Level.Info -> Pretty.Info
  | Level.Warn -> Pretty.Warn
  | Level.Error -> Pretty.Error

let add_header renderer ~scope log =
  Pretty.header renderer
    ~unix_ns:(Timestamp.to_unix_ns (Log.timestamp log))
    ~severity:(pretty_severity (Log.level log))
    ~scope

let pretty_error = function
  | Pretty.Invalid_utf8 -> Invalid_utf8
  | Pretty.Non_finite_float -> Non_finite_float
  | Pretty.Unsupported_value -> Unsupported_value
  | Pretty.Malformed -> Failed

let pretty_capacity = function
  | Plain -> 512
  | Ansi_16 -> 1_024
  | Ansi_256 -> 1_280
  | Truecolor -> 1_792

let pretty_with_line_feed line_feed style =
  create (fun log ->
      try
        let renderer =
          match Log.body log with
          | Text _ -> Pretty.create ~capacity:128 style
          | Untyped _ | Typed _ ->
              Pretty.create ~capacity:(pretty_capacity style) style
        in
        (match Log.body log with
        | Text { tag; message } ->
            add_header renderer ~scope:tag log;
            Pretty.space renderer;
            Pretty.text renderer message
        | Untyped value ->
            add_header renderer ~scope:(Log.service log) log;
            Pretty.newline renderer;
            Value.append_pretty renderer Pretty.Root value
        | Typed (description, value) ->
            add_header renderer ~scope:(Log.service log) log;
            Pretty.newline renderer;
            Type.pretty description renderer Pretty.Root value);
        if line_feed then Pretty.newline renderer;
        Ok (Pretty.contents renderer)
      with Pretty.Error error -> Error (pretty_error error))

let pretty = pretty_with_line_feed false
let pretty_line = pretty_with_line_feed true

exception Json_failure of error

let append_value buffer value =
  match Value.append_json buffer value with
  | Ok () -> ()
  | Error Value.Invalid_utf8 -> raise (Json_failure Invalid_utf8)
  | Error Value.Non_finite_float -> raise (Json_failure Non_finite_float)
  | Error Value.Unsupported_value -> raise (Json_failure Unsupported_value)

let append_body_json buffer log =
  match Log.body log with
  | Text { tag; message } ->
      Buffer.add_string buffer "{\"kind\":\"text\",\"tag\":";
      Json_writer.string buffer tag;
      Buffer.add_string buffer ",\"message\":";
      Json_writer.string buffer message;
      Buffer.add_char buffer '}'
  | Untyped value -> append_value buffer value
  | Typed (description, value) -> Type.append_json buffer description value

let encode_json ~line_feed log =
  try
    let buffer = Buffer.create 512 in
    Buffer.add_string buffer "{\"service\":";
    Json_writer.string buffer (Log.service log);
    (match Log.environment log with
    | None -> ()
    | Some environment ->
        Buffer.add_string buffer ",\"environment\":";
        Json_writer.string buffer environment);
    (match Log.version log with
    | None -> ()
    | Some version ->
        Buffer.add_string buffer ",\"version\":";
        Json_writer.string buffer version);
    Buffer.add_string buffer ",\"timestamp\":\"";
    Json_writer.decimal_int64 buffer (Timestamp.to_unix_ns (Log.timestamp log));
    Buffer.add_string buffer "\",\"level\":\"";
    Buffer.add_string buffer (Level.to_string (Log.level log));
    Buffer.add_string buffer "\",\"body\":";
    append_body_json buffer log;
    Buffer.add_char buffer '}';
    if line_feed then Buffer.add_char buffer '\n';
    Ok (Buffer.contents buffer)
  with
  | Json_writer.Invalid_utf8 -> Error Invalid_utf8
  | Json_failure error -> Error error
  | Repr.Unsupported_operation _ -> Error Unsupported_value

let json = create (encode_json ~line_feed:false)
let ndjson = create (encode_json ~line_feed:true)
