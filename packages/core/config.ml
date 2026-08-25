type t = {
  service : string;
  environment : string option;
  version : string option;
  enabled : bool;
  console : console;
  min_level : Level.t;
  drains : Drain.t list;
  enrichers : Log_enricher.t list;
  limits : Log_limits.t;
}

and console = Auto | Pretty | Ndjson | Silent

type field = Service | Environment | Version | Enrichers

type problem =
  | Empty
  | Invalid_utf8
  | Duplicate_enricher_name of string
  | Overlapping_authoritative_field of string

type error = { field : field; problem : problem }

exception Invalid_configuration of error

module String_set = Set.Make (String)

let validate field value =
  if String.length value = 0 then Error { field; problem = Empty }
  else if not (Utf8.is_valid value) then Error { field; problem = Invalid_utf8 }
  else Ok ()

let validate_optional field = function
  | None -> Ok ()
  | Some value -> validate field value

let copy_string value = Bytes.unsafe_to_string (Bytes.of_string value)
let copy_optional = Option.map copy_string

let rec validate_authoritative_fields seen = function
  | [] -> Ok seen
  | field :: rest ->
      if String_set.mem field seen then
        Error
          { field = Enrichers; problem = Overlapping_authoritative_field field }
      else validate_authoritative_fields (String_set.add field seen) rest

let validate_enrichers enrichers =
  let rec loop names authoritative = function
    | [] -> Ok ()
    | enricher :: rest -> (
        let name = Log_enricher.name enricher in
        if String_set.mem name names then
          Error { field = Enrichers; problem = Duplicate_enricher_name name }
        else
          match
            validate_authoritative_fields authoritative
              (Log_enricher.authoritative_fields enricher)
          with
          | Error _ as error -> error
          | Ok authoritative ->
              loop (String_set.add name names) authoritative rest)
  in
  loop String_set.empty String_set.empty enrichers

let sort_enrichers =
  List.sort (fun left right ->
      String.compare (Log_enricher.name left) (Log_enricher.name right))

let create ~service ?environment ?version ?(enabled = true) ?(console = Auto)
    ?(min_level = Level.Info) ?(drains = []) ?(enrichers = [])
    ?(limits = Log_limits.default) () =
  match validate Service service with
  | Error _ as error -> error
  | Ok () -> (
      match validate_optional Environment environment with
      | Error _ as error -> error
      | Ok () -> (
          match validate_optional Version version with
          | Error _ as error -> error
          | Ok () -> (
              match validate_enrichers enrichers with
              | Error _ as error -> error
              | Ok () ->
                  Ok
                    {
                      service = copy_string service;
                      environment = copy_optional environment;
                      version = copy_optional version;
                      enabled;
                      console;
                      min_level;
                      drains;
                      enrichers = sort_enrichers enrichers;
                      limits;
                    })))

let pp_field formatter = function
  | Service -> Format.pp_print_string formatter "service"
  | Environment -> Format.pp_print_string formatter "environment"
  | Version -> Format.pp_print_string formatter "version"
  | Enrichers -> Format.pp_print_string formatter "enrichers"

let pp_problem formatter = function
  | Empty -> Format.pp_print_string formatter "must not be empty"
  | Invalid_utf8 -> Format.pp_print_string formatter "must be valid UTF-8"
  | Duplicate_enricher_name name ->
      Format.fprintf formatter "duplicate enricher name %S" name
  | Overlapping_authoritative_field name ->
      Format.fprintf formatter "overlapping authoritative field %S" name

let pp_error formatter { field; problem } =
  Format.fprintf formatter "%a %a" pp_field field pp_problem problem

let create_exn ~service ?environment ?version ?enabled ?console ?min_level
    ?drains ?enrichers ?limits () =
  match
    create ~service ?environment ?version ?enabled ?console ?min_level ?drains
      ?enrichers ?limits ()
  with
  | Ok config -> config
  | Error error -> raise (Invalid_configuration error)

let service t = t.service
let environment t = t.environment
let version t = t.version
let enabled t = t.enabled
let console t = t.console
let min_level t = t.min_level
let drains t = t.drains
let enrichers t = t.enrichers
let limits t = t.limits
