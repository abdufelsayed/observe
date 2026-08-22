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
  let rec inspect_expression expression =
    (match expression.pexp_desc with
    | Pexp_apply ({ pexp_desc = Pexp_ident { txt = path; _ }; _ }, arguments) ->
        record calls
          (call_key (longident_name path) (first_string_argument arguments))
    | Pexp_field (_, { txt = Lident field; _ }) -> record fields field
    | _ -> ());
    let inspect_case case =
      Option.iter inspect_expression case.pc_guard;
      inspect_expression case.pc_rhs
    in
    let inspect_binding binding = inspect_expression binding.pvb_expr in
    match expression.pexp_desc with
    | Pexp_let (_, nested, body) ->
        List.iter inspect_binding nested;
        inspect_expression body
    | Pexp_function (parameters, _, body) -> (
        List.iter
          (fun parameter ->
            match parameter.pparam_desc with
            | Pparam_val (_, default, _) ->
                Option.iter inspect_expression default
            | Pparam_newtype _ -> ())
          parameters;
        match body with
        | Pfunction_body body -> inspect_expression body
        | Pfunction_cases (cases, _, _) -> List.iter inspect_case cases)
    | Pexp_apply (function_, arguments) ->
        inspect_expression function_;
        List.iter (fun (_, argument) -> inspect_expression argument) arguments
    | Pexp_match (value, cases) | Pexp_try (value, cases) ->
        inspect_expression value;
        List.iter inspect_case cases
    | Pexp_tuple values | Pexp_array values ->
        List.iter inspect_expression values
    | Pexp_construct (_, value) | Pexp_variant (_, value) ->
        Option.iter inspect_expression value
    | Pexp_record (fields, base) ->
        List.iter (fun (_, value) -> inspect_expression value) fields;
        Option.iter inspect_expression base
    | Pexp_field (value, _)
    | Pexp_send (value, _)
    | Pexp_constraint (value, _)
    | Pexp_assert value
    | Pexp_lazy value
    | Pexp_poly (value, _)
    | Pexp_newtype (_, value)
    | Pexp_open (_, value) ->
        inspect_expression value
    | Pexp_setfield (record_, _, value) ->
        inspect_expression record_;
        inspect_expression value
    | Pexp_ifthenelse (condition, yes, no) ->
        inspect_expression condition;
        inspect_expression yes;
        Option.iter inspect_expression no
    | Pexp_sequence (first, second) | Pexp_while (first, second) ->
        inspect_expression first;
        inspect_expression second
    | Pexp_for (_, first, last, _, body) ->
        inspect_expression first;
        inspect_expression last;
        inspect_expression body
    | Pexp_coerce (value, _, _) | Pexp_setinstvar (_, value) ->
        inspect_expression value
    | Pexp_override fields ->
        List.iter (fun (_, value) -> inspect_expression value) fields
    | Pexp_letmodule (_, _, body) | Pexp_letexception (_, body) ->
        inspect_expression body
    | Pexp_letop letop ->
        inspect_expression letop.let_.pbop_exp;
        List.iter
          (fun binding -> inspect_expression binding.pbop_exp)
          letop.ands;
        inspect_expression letop.body
    | Pexp_ident _ | Pexp_constant _ | Pexp_new _ | Pexp_object _ | Pexp_pack _
    | Pexp_extension _ | Pexp_unreachable ->
        ()
  in
  let rec inspect_structure structure =
    List.iter inspect_structure_item structure
  and inspect_structure_item item =
    match item.pstr_desc with
    | Pstr_value (_, value_bindings) ->
        List.iter
          (fun binding ->
            (match binding.pvb_pat.ppat_desc with
            | Ppat_var { txt = name; _ } -> record bindings name
            | _ -> ());
            inspect_expression binding.pvb_expr)
          value_bindings
    | Pstr_module binding -> inspect_module binding.pmb_expr
    | Pstr_recmodule bindings ->
        List.iter (fun binding -> inspect_module binding.pmb_expr) bindings
    | Pstr_include include_ -> inspect_module include_.pincl_mod
    | Pstr_eval (expression, _) -> inspect_expression expression
    | _ -> ()
  and inspect_module module_ =
    match module_.pmod_desc with
    | Pmod_structure structure -> inspect_structure structure
    | Pmod_functor (_, body) -> inspect_module body
    | Pmod_apply (function_, argument) ->
        inspect_module function_;
        inspect_module argument
    | Pmod_apply_unit function_ -> inspect_module function_
    | Pmod_constraint (module_, _) -> inspect_module module_
    | Pmod_unpack expression -> inspect_expression expression
    | Pmod_ident _ | Pmod_extension _ -> ()
  in
  inspect_structure structure;
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
      require calls "Observe.Type.sealr" 2;
      require calls "Observe.Type.|+" 4;
      require calls "Observe.Type.record User_login" 2;
      require calls "Observe.Type.field user_id" 2;
      require calls "Observe.Type.field method_" 2;
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
      require calls "Observe.Logs.log" 1;
      require fields "text" 2;
      require fields "value" 1;
      require fields "untyped" 0;
      require fields "field" 0;
      require fields "seal" 0;
      require fields "typed" 2
  | Some mode -> fail "unknown expansion mode %S" mode
