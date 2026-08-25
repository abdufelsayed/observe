module String_set = Set.Make (String)

type error =
  | Empty_name
  | Invalid_name
  | Empty_authoritative_field
  | Invalid_authoritative_field
  | Duplicate_authoritative_field of string
  | Reserved_authoritative_field of string

exception Invalid_enricher of error

type t = {
  name : string;
  authoritative_fields : string list;
  authority : String_set.t;
  author : unit -> Value.t;
}

let copy_string value = Bytes.unsafe_to_string (Bytes.of_string value)

let validate_name name =
  if String.length name = 0 then Error Empty_name
  else if not (Utf8.is_valid name) then Error Invalid_name
  else Ok (copy_string name)

let validate_authoritative_fields fields =
  let rec validate seen validated = function
    | [] -> Ok (List.rev validated, seen)
    | field :: rest ->
        if String.length field = 0 then Error Empty_authoritative_field
        else if not (Utf8.is_valid field) then Error Invalid_authoritative_field
        else if Log_envelope.is_reserved_field field then
          Error (Reserved_authoritative_field (copy_string field))
        else if String_set.mem field seen then
          Error (Duplicate_authoritative_field (copy_string field))
        else
          let field = copy_string field in
          validate (String_set.add field seen) (field :: validated) rest
  in
  validate String_set.empty [] fields

let create ~name ?(authoritative_fields = []) author =
  match validate_name name with
  | Error _ as error -> error
  | Ok name -> (
      match validate_authoritative_fields authoritative_fields with
      | Error _ as error -> error
      | Ok (authoritative_fields, authority) ->
          Ok { name; authoritative_fields; authority; author })

let create_exn ~name ?authoritative_fields author =
  match create ~name ?authoritative_fields author with
  | Ok enricher -> enricher
  | Error error -> raise (Invalid_enricher error)

let name enricher = enricher.name
let authoritative_fields enricher = enricher.authoritative_fields
let is_authoritative enricher field = String_set.mem field enricher.authority
let author enricher = enricher.author

let pp_error formatter = function
  | Empty_name -> Format.pp_print_string formatter "name must not be empty"
  | Invalid_name -> Format.pp_print_string formatter "name must be valid UTF-8"
  | Empty_authoritative_field ->
      Format.pp_print_string formatter "authoritative field must not be empty"
  | Invalid_authoritative_field ->
      Format.pp_print_string formatter "authoritative field must be valid UTF-8"
  | Duplicate_authoritative_field field ->
      Format.fprintf formatter "authoritative field %S is duplicated" field
  | Reserved_authoritative_field field ->
      Format.fprintf formatter "authoritative field %S is reserved" field
