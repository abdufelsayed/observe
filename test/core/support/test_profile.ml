let positive_int_env ~name ~default =
  match Sys.getenv_opt name with
  | None | Some "" -> default
  | Some value -> (
      match int_of_string_opt value with
      | Some count when count > 0 -> count
      | _ -> default)

let qcheck_count ~default =
  positive_int_env ~name:"OBSERVE_QCHECK_COUNT" ~default
