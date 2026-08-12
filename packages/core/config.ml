type t = {
  service : string;
  environment : string option;
  version : string option;
  enabled : bool;
  console : console;
  min_level : Level.t;
  drains : Drain.t list;
}

and console = Auto | Pretty | Ndjson | Silent

type field = Service | Environment | Version
type problem = Empty | Invalid_utf8
type error = { field : field; problem : problem }

exception Invalid_configuration of error

let validate field value =
  if String.length value = 0 then Error { field; problem = Empty }
  else if not (Utf8.is_valid value) then Error { field; problem = Invalid_utf8 }
  else Ok ()

let validate_optional field = function
  | None -> Ok ()
  | Some value -> validate field value

let create ~service ?environment ?version ?(enabled = true) ?(console = Auto)
    ?(min_level = Level.Info) ?(drains = []) () =
  match validate Service service with
  | Error _ as error -> error
  | Ok () -> (
      match validate_optional Environment environment with
      | Error _ as error -> error
      | Ok () -> (
          match validate_optional Version version with
          | Error _ as error -> error
          | Ok () ->
              Ok
                {
                  service;
                  environment;
                  version;
                  enabled;
                  console;
                  min_level;
                  drains;
                }))

let pp_field formatter = function
  | Service -> Format.pp_print_string formatter "service"
  | Environment -> Format.pp_print_string formatter "environment"
  | Version -> Format.pp_print_string formatter "version"

let pp_problem formatter = function
  | Empty -> Format.pp_print_string formatter "must not be empty"
  | Invalid_utf8 -> Format.pp_print_string formatter "must be valid UTF-8"

let pp_error formatter { field; problem } =
  Format.fprintf formatter "%a %a" pp_field field pp_problem problem

let create_exn ~service ?environment ?version ?enabled ?console ?min_level
    ?drains () =
  match
    create ~service ?environment ?version ?enabled ?console ?min_level ?drains
      ()
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
