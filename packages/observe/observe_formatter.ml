type error = Invalid_utf8 | Non_finite_float | Unsupported_value | Failed
type t = Observe_log.t -> (string, error) result

let create formatter = formatter
let format formatter log = formatter log

let metadata_prefix log =
  let environment =
    match Observe_log.environment log with
    | None -> ""
    | Some value -> " environment=" ^ value
  in
  let version =
    match Observe_log.version log with
    | None -> ""
    | Some value -> " version=" ^ value
  in
  Format.asprintf "%a %s%s%s %s" Observe_instant.pp (Observe_log.instant log)
    (Observe_log.service log) environment version
    (Observe_level.to_string (Observe_log.level log))

let readable =
  create (fun log ->
      let prefix = metadata_prefix log in
      match Observe_log.payload log with
      | Text { tag; message } -> Ok (prefix ^ " [" ^ tag ^ "] " ^ message)
      | Free value -> Ok (Format.asprintf "%s %a" prefix Observe_value.pp value)
      | Structured (description, value) ->
          Ok (Format.asprintf "%s %a" prefix (Repr.pp description) value))

let json_error = function
  | Observe_value.Invalid_utf8 -> Invalid_utf8
  | Observe_value.Non_finite_float -> Non_finite_float
  | Observe_value.Unsupported_value -> Unsupported_value

let json_value value =
  Result.map_error json_error (Observe_value.to_json_string value)

let json_string value = json_value (Observe_value.string value)

let json_object fields =
  let buffer = Buffer.create 128 in
  Buffer.add_char buffer '{';
  List.iteri
    (fun index (name, encoded_value) ->
      if index <> 0 then Buffer.add_char buffer ',';
      Buffer.add_char buffer '"';
      Buffer.add_string buffer name;
      Buffer.add_string buffer "\":";
      Buffer.add_string buffer encoded_value)
    fields;
  Buffer.add_char buffer '}';
  Buffer.contents buffer

let text_json ~tag ~message =
  match json_string tag with
  | Error _ as error -> error
  | Ok tag -> (
      match json_string message with
      | Error _ as error -> error
      | Ok message ->
          Ok
            (json_object
               [ ("kind", "\"text\""); ("tag", tag); ("message", message) ]))

let payload_json log =
  match Observe_log.payload log with
  | Text { tag; message } -> text_json ~tag ~message
  | Free value -> json_value value
  | Structured (description, value) ->
      Ok (Repr.to_json_string ~minify:true description value)

let optional_string_field name = function
  | None -> Ok []
  | Some value -> (
      match json_string value with
      | Error _ as error -> error
      | Ok value -> Ok [ (name, value) ])

let encode_json log =
  match json_string (Observe_log.service log) with
  | Error _ as error -> error
  | Ok service -> (
      match
        optional_string_field "environment" (Observe_log.environment log)
      with
      | Error _ as error -> error
      | Ok environment -> (
          match optional_string_field "version" (Observe_log.version log) with
          | Error _ as error -> error
          | Ok version -> (
              match
                json_string
                  (Int64.to_string
                     (Observe_instant.to_epoch_nanoseconds
                        (Observe_log.instant log)))
              with
              | Error _ as error -> error
              | Ok instant -> (
                  match
                    json_string
                      (Observe_level.to_string (Observe_log.level log))
                  with
                  | Error _ as error -> error
                  | Ok level -> (
                      match payload_json log with
                      | Error _ as error -> error
                      | Ok payload ->
                          Ok
                            (json_object
                               ([ ("service", service) ]
                               @ environment
                               @ version
                               @ [
                                   ("instant", instant);
                                   ("level", level);
                                   ("payload", payload);
                                 ])))))))

let json = create encode_json

let json_lines =
  create (fun log ->
      match encode_json log with
      | Error _ as error -> error
      | Ok encoded -> Ok (encoded ^ "\n"))
