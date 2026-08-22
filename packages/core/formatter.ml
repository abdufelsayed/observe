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

let add_point_correlation renderer log =
  match Log.correlation_id log with
  | None -> ()
  | Some id ->
      Pretty.space renderer;
      Pretty.trusted_text renderer "(";
      Pretty.trusted_text renderer id;
      Pretty.trusted_text renderer ")"

let add_operation_context renderer operation =
  Pretty.space renderer;
  Pretty.duration renderer (Log.operation_duration_ns operation);
  Pretty.space renderer;
  Pretty.trusted_text renderer "(";
  Pretty.trusted_text renderer (Log.operation_id operation);
  Option.iter
    (fun parent_id ->
      Pretty.trusted_text renderer " <- ";
      Pretty.trusted_text renderer parent_id)
    (Log.operation_parent_id operation);
  Pretty.trusted_text renderer ")"

let pretty_with_line_feed line_feed style =
  create (fun log ->
      try
        let renderer =
          match Log.body log with
          | Text _ -> Pretty.create ~capacity:128 style
          | Structured _ ->
              Pretty.create ~capacity:(pretty_capacity style) style
        in
        (match (Log.operation log, Log.body log) with
        | None, Text { tag; message } ->
            add_header renderer ~scope:tag log;
            add_point_correlation renderer log;
            Pretty.space renderer;
            Pretty.trusted_text renderer message
        | None, Structured { value; _ } ->
            add_header renderer ~scope:(Log.service log) log;
            add_point_correlation renderer log;
            Pretty.newline renderer;
            Value.append_frozen_pretty renderer Pretty.Root value
        | Some operation, Text { message; _ } ->
            add_header renderer ~scope:(Log.operation_name operation) log;
            add_operation_context renderer operation;
            Pretty.space renderer;
            Pretty.trusted_text renderer message
        | Some operation, Structured { value; _ } ->
            add_header renderer ~scope:(Log.operation_name operation) log;
            add_operation_context renderer operation;
            Pretty.newline renderer;
            Value.append_frozen_pretty renderer Pretty.Root value);
        if line_feed then Pretty.newline renderer;
        Ok (Pretty.contents renderer)
      with Pretty.Error error -> Error (pretty_error error))

let pretty = pretty_with_line_feed false
let pretty_line = pretty_with_line_feed true

let append_body_json buffer log =
  match Log.body log with
  | Text { tag; message } ->
      Buffer.add_string buffer "{\"kind\":\"text\",\"tag\":";
      Json_writer.trusted_string buffer tag;
      Buffer.add_string buffer ",\"message\":";
      Json_writer.trusted_string buffer message;
      Buffer.add_char buffer '}'
  | Structured { value; _ } -> Value.append_frozen_json buffer value

let append_correlation_json buffer log =
  match Log.correlation_id log with
  | None -> ()
  | Some id ->
      Buffer.add_string buffer ",\"operation_id\":";
      Json_writer.trusted_string buffer id

let append_operation_json buffer operation =
  Buffer.add_string buffer ",\"operation\":{\"name\":";
  Json_writer.trusted_string buffer (Log.operation_name operation);
  Buffer.add_string buffer ",\"id\":";
  Json_writer.trusted_string buffer (Log.operation_id operation);
  (match Log.operation_parent_id operation with
  | None -> ()
  | Some parent_id ->
      Buffer.add_string buffer ",\"parent_id\":";
      Json_writer.trusted_string buffer parent_id);
  Buffer.add_string buffer ",\"duration_ns\":\"";
  Json_writer.decimal_int64 buffer (Log.operation_duration_ns operation);
  Buffer.add_string buffer "\"}"

let append_json_object buffer log =
  Buffer.add_string buffer "{\"service\":";
  Json_writer.trusted_string buffer (Log.service log);
  (match Log.environment log with
  | None -> ()
  | Some environment ->
      Buffer.add_string buffer ",\"environment\":";
      Json_writer.trusted_string buffer environment);
  (match Log.version log with
  | None -> ()
  | Some version ->
      Buffer.add_string buffer ",\"version\":";
      Json_writer.trusted_string buffer version);
  Buffer.add_string buffer ",\"timestamp\":\"";
  Json_writer.decimal_int64 buffer (Timestamp.to_unix_ns (Log.timestamp log));
  Buffer.add_string buffer "\",\"level\":\"";
  Buffer.add_string buffer (Level.to_string (Log.level log));
  Buffer.add_char buffer '"';
  (match Log.operation log with
  | None -> append_correlation_json buffer log
  | Some operation -> append_operation_json buffer operation);
  Buffer.add_string buffer ",\"body\":";
  append_body_json buffer log;
  Buffer.add_char buffer '}'

let encode_json ~line_feed log =
  try
    let buffer = Buffer.create 512 in
    append_json_object buffer log;
    if line_feed then Buffer.add_char buffer '\n';
    Ok (Buffer.contents buffer)
  with
  | Json_writer.Invalid_utf8 -> Error Invalid_utf8
  | Repr.Unsupported_operation _ -> Error Unsupported_value

let json = create (encode_json ~line_feed:false)
let ndjson = create (encode_json ~line_feed:true)
