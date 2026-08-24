type roles = {
  kind : string option;
  code : string option;
  message : string option;
  explanation : string option;
  remediation : string option;
  documentation : string option;
}

type 'error t = 'error -> roles

let roles ?kind ?code ?message ?explanation ?remediation ?documentation () =
  { kind; code; message; explanation; remediation; documentation }

let create interpret = interpret

let exn error =
  roles
    ~kind:(Printexc.exn_slot_name error)
    ~message:(Printexc.to_string error) ()

let value interpret ?backtrace error =
  let interpreted = interpret error in
  let fields =
    [
      ("kind", interpreted.kind);
      ("code", interpreted.code);
      ("message", interpreted.message);
      ("explanation", interpreted.explanation);
      ("remediation", interpreted.remediation);
      ("documentation", interpreted.documentation);
      ("backtrace", Option.map Printexc.raw_backtrace_to_string backtrace);
    ]
    |> List.filter_map (fun (name, value) ->
        Option.map (fun value -> (name, Value.string value)) value)
  in
  Value.object_ [ ("error", Value.object_ fields) ]

let freeze interpret ?backtrace error =
  Value.freeze (value interpret ?backtrace error)

let freeze_into interpret ?backtrace error context ~depth =
  Value.freeze_into (value interpret ?backtrace error) context ~depth
