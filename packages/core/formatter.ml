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

let render_string_field renderer ~last ~name value =
  let nested = Pretty.field renderer ~last ~name ~scalar:true in
  Pretty.trusted_string renderer value;
  Pretty.finish renderer nested

let render_text_field renderer ~last ~name value =
  let nested = Pretty.field renderer ~last ~name ~scalar:true in
  Pretty.trusted_text renderer value;
  Pretty.finish renderer nested

let operation_label reference =
  Log.operation_reference_name reference
  ^ " ("
  ^ Log.operation_reference_id reference
  ^ ")"

let render_annotations renderer annotations =
  let nested = Pretty.field renderer ~last:true ~name:"logs" ~scalar:false in
  let length = List.length annotations in
  List.iteri
    (fun index annotation ->
      let item =
        Pretty.index renderer ~last:(index = length - 1) ~index ~scalar:false
      in
      render_string_field renderer ~last:false ~name:"timestamp"
        (Timestamp.to_rfc3339 (Log.annotation_timestamp annotation));
      Pretty.newline renderer;
      render_string_field renderer ~last:false ~name:"level"
        (Level.to_string (Log.annotation_level annotation));
      Pretty.newline renderer;
      render_string_field renderer ~last:true ~name:"message"
        (Log.annotation_message annotation);
      Pretty.finish renderer item;
      if index < length - 1 then Pretty.newline renderer)
    annotations;
  Pretty.finish renderer nested

let render_point_correlation renderer reference =
  Pretty.newline renderer;
  render_text_field renderer ~last:true ~name:"operation"
    (operation_label reference)

let render_structured_point renderer log value =
  let correlation = Log.correlation log in
  let event_fields = Snapshot.root_field_count value in
  Pretty.newline renderer;
  Option.iter
    (fun reference ->
      render_text_field renderer ~last:false ~name:"operation"
        (operation_label reference);
      Pretty.newline renderer)
    correlation;
  if event_fields = 0 then (
    let nested = Pretty.place renderer Pretty.Root ~scalar:true in
    Pretty.empty_record renderer;
    Pretty.finish renderer nested)
  else Snapshot.append_root_pretty_fields renderer ~trailing:0 value

let render_wide renderer operation value annotations =
  Pretty.space renderer;
  Pretty.duration renderer (Log.operation_duration_ns operation);
  Pretty.newline renderer;
  let parent = Log.operation_parent operation in
  let event_fields = Snapshot.root_field_count value in
  let has_logs = annotations <> [] in
  let after_id =
    (if Option.is_some parent then 1 else 0)
    + event_fields
    + if has_logs then 1 else 0
  in
  render_string_field renderer ~last:(after_id = 0) ~name:"id"
    (Log.operation_id operation);
  Option.iter
    (fun reference ->
      Pretty.newline renderer;
      render_text_field renderer
        ~last:(event_fields = 0 && not has_logs)
        ~name:"parent"
        (operation_label reference))
    parent;
  if event_fields > 0 then (
    Pretty.newline renderer;
    Snapshot.append_root_pretty_fields renderer
      ~trailing:(if has_logs then 1 else 0)
      value);
  if has_logs then (
    Pretty.newline renderer;
    render_annotations renderer annotations)

let pretty_with_line_feed line_feed style =
  create (fun log ->
      try
        let renderer =
          match Log.event log with
          | Text _ -> Pretty.create ~capacity:256 style
          | Structured _ ->
              Pretty.create ~capacity:(pretty_capacity style) style
        in
        (match (Log.operation log, Log.event log) with
        | None, Text { tag; message } ->
            add_header renderer ~scope:tag log;
            Pretty.space renderer;
            Pretty.trusted_text renderer message;
            Option.iter
              (render_point_correlation renderer)
              (Log.correlation log)
        | None, Structured { value; _ } ->
            add_header renderer ~scope:(Log.service log) log;
            render_structured_point renderer log value
        | Some operation, Structured { value; _ } ->
            add_header renderer ~scope:(Log.operation_name operation) log;
            render_wide renderer operation value (Log.annotations log)
        | Some _, Text _ -> assert false);
        if line_feed then Pretty.newline renderer;
        Ok (Pretty.contents renderer)
      with
      | Pretty.Error error -> Error (pretty_error error)
      | Invalid_argument _ -> Error Failed)

let pretty = pretty_with_line_feed false
let pretty_line = pretty_with_line_feed true

let append_named_string buffer ~first name value =
  if not first then Buffer.add_char buffer ',';
  Json_writer.trusted_name buffer name;
  Json_writer.trusted_string buffer value;
  false

let append_timestamp buffer ~first name value =
  if not first then Buffer.add_char buffer ',';
  Json_writer.trusted_name buffer name;
  Buffer.add_char buffer '"';
  Timestamp.append_rfc3339 buffer value;
  Buffer.add_char buffer '"';
  false

let append_duration_ms buffer duration_ns =
  let duration_ns =
    if Int64.compare duration_ns 0L < 0 then 0L else duration_ns
  in
  let whole = Int64.div duration_ns 1_000_000L in
  let remainder = Int64.to_int (Int64.rem duration_ns 1_000_000L) in
  Json_writer.decimal_int64 buffer whole;
  if remainder <> 0 then (
    let digits = Bytes.make 6 '0' in
    let rec fill index value =
      if index >= 0 then (
        Bytes.set digits index (Char.unsafe_chr (48 + (value mod 10)));
        fill (index - 1) (value / 10))
    in
    fill 5 remainder;
    let last = ref 5 in
    while !last > 0 && Bytes.get digits !last = '0' do
      decr last
    done;
    Buffer.add_char buffer '.';
    Buffer.add_subbytes buffer digits 0 (!last + 1))

let append_operation_fields buffer ~first operation =
  let first =
    append_named_string buffer ~first "operation" (Log.operation_name operation)
  in
  let first =
    append_named_string buffer ~first "operation_id"
      (Log.operation_id operation)
  in
  let first =
    match Log.operation_parent operation with
    | None -> first
    | Some parent ->
        let first =
          append_named_string buffer ~first "parent_operation"
            (Log.operation_reference_name parent)
        in
        append_named_string buffer ~first "parent_operation_id"
          (Log.operation_reference_id parent)
  in
  if not first then Buffer.add_char buffer ',';
  Json_writer.trusted_name buffer "duration_ms";
  append_duration_ms buffer (Log.operation_duration_ns operation);
  false

let append_correlation_fields buffer ~first reference =
  let first =
    append_named_string buffer ~first "operation"
      (Log.operation_reference_name reference)
  in
  append_named_string buffer ~first "operation_id"
    (Log.operation_reference_id reference)

let append_event_fields buffer ~first = function
  | Log.Text { tag; message } ->
      let first = append_named_string buffer ~first "tag" tag in
      append_named_string buffer ~first "message" message
  | Log.Structured { value; _ } ->
      Snapshot.append_root_json_fields buffer ~first value

let append_annotations buffer ~first annotations =
  if annotations = [] then first
  else (
    if not first then Buffer.add_char buffer ',';
    Json_writer.trusted_name buffer "logs";
    Buffer.add_char buffer '[';
    List.iteri
      (fun index annotation ->
        if index > 0 then Buffer.add_char buffer ',';
        Buffer.add_char buffer '{';
        ignore
          (append_timestamp buffer ~first:true "timestamp"
             (Log.annotation_timestamp annotation));
        ignore
          (append_named_string buffer ~first:false "level"
             (Level.to_string (Log.annotation_level annotation)));
        ignore
          (append_named_string buffer ~first:false "message"
             (Log.annotation_message annotation));
        Buffer.add_char buffer '}')
      annotations;
    Buffer.add_char buffer ']';
    false)

let append_json_object buffer log =
  Buffer.add_char buffer '{';
  let first =
    append_named_string buffer ~first:true "service" (Log.service log)
  in
  let first =
    match Log.environment log with
    | None -> first
    | Some environment ->
        append_named_string buffer ~first "environment" environment
  in
  let first =
    match Log.version log with
    | None -> first
    | Some version -> append_named_string buffer ~first "version" version
  in
  let first = append_timestamp buffer ~first "timestamp" (Log.timestamp log) in
  let first =
    append_named_string buffer ~first "level" (Level.to_string (Log.level log))
  in
  let first =
    match (Log.operation log, Log.correlation log) with
    | Some operation, _ -> append_operation_fields buffer ~first operation
    | None, Some reference -> append_correlation_fields buffer ~first reference
    | None, None -> first
  in
  let first = append_event_fields buffer ~first (Log.event log) in
  ignore (append_annotations buffer ~first (Log.annotations log));
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
  | Invalid_argument _ -> Error Failed

let json = create (encode_json ~line_feed:false)
let ndjson = create (encode_json ~line_feed:true)
