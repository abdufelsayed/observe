open Ppxlib

let rec longident_name = function
  | Longident.Lident name -> name
  | Ldot (path, name) -> longident_name path ^ "." ^ name
  | Lapply (function_, argument) ->
      longident_name function_ ^ "(" ^ longident_name argument ^ ")"

let first_string_argument arguments =
  List.find_map
    (fun (_, expression) ->
      match expression.pexp_desc with
      | Pexp_constant (Pconst_string (value, _, _)) -> Some value
      | _ -> None)
    arguments

let call_key path argument =
  match argument with None -> path | Some value -> path ^ " " ^ value

let fail format = Format.kasprintf failwith format

let () =
  if Array.length Sys.argv < 2 || Array.length Sys.argv > 3 then
    fail "usage: %s EXPANSION [recursive]" Sys.argv.(0);
  let channel = open_in_bin Sys.argv.(1) in
  let lexbuf = Lexing.from_channel channel in
  Lexing.set_filename lexbuf Sys.argv.(1);
  let structure =
    Fun.protect
      ~finally:(fun () -> close_in channel)
      (fun () -> Parse.implementation lexbuf)
  in
  let bindings = Hashtbl.create 1 in
  let calls = Hashtbl.create 16 in
  let fields = Hashtbl.create 4 in
  let record table key =
    Hashtbl.replace table key
      (1 + Option.value ~default:0 (Hashtbl.find_opt table key))
  in
  let inspector =
    object
      inherit Ast_traverse.iter as super

      method! value_binding binding =
        (match binding.pvb_pat.ppat_desc with
        | Ppat_var { txt = name; _ } -> record bindings name
        | _ -> ());
        super#value_binding binding

      method! expression expression =
        (match expression.pexp_desc with
        | Pexp_apply ({ pexp_desc = Pexp_ident { txt = path; _ }; _ }, arguments)
          ->
            record calls
              (call_key (longident_name path) (first_string_argument arguments))
        | Pexp_field (_, { txt = Lident field; _ }) -> record fields field
        | _ -> ());
        super#expression expression
    end
  in
  inspector#structure structure;
  let require table key expected =
    let actual = Option.value ~default:0 (Hashtbl.find_opt table key) in
    if actual <> expected then
      fail "expected %S %d time(s), found %d" key expected actual
  in
  match if Array.length Sys.argv = 3 then Some Sys.argv.(2) else None with
  | None ->
      require bindings "event_t" 1;
      require calls "Observe.Generated_runtime.with_plan" 1;
      require calls "Observe.Type.sealv" 1;
      require calls "Observe.Type.|~" 2;
      require calls "Observe.Type.variant event" 1;
      require calls "Observe.Type.case1 User_login" 1;
      require calls "Observe.Type.sealr" 1;
      require calls "Observe.Type.|+" 2;
      require calls "Observe.Type.record User_login" 1;
      require calls "Observe.Type.field user_id" 1;
      require calls "Observe.Type.field method_" 1;
      require calls "Observe.Type.case0 Idle" 1
  | Some "recursive" ->
      require bindings "node_t" 1;
      require calls "Observe.Generated_runtime.with_recursive_plan" 1;
      require calls "Observe.Generated_runtime.constructor Leaf" 1;
      require calls "Observe.Generated_runtime.constructor Branch" 1
  | Some "logging" ->
      require calls "Observe.Logs.debug" 1;
      require calls "Observe.Logs.info" 1;
      require calls "Observe.Logs.warn" 1;
      require calls "Observe.Logs.error" 1;
      require calls "Observe.Logs.emit" 1;
      require fields "text" 2;
      require fields "untyped" 1;
      require fields "typed" 2
  | Some mode -> fail "unknown expansion mode %S" mode
