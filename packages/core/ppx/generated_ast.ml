open Ppxlib
open Ast_builder.Default

let lident ~loc name = Located.mk ~loc (Longident.parse name)
let evar ~loc name = pexp_ident ~loc (lident ~loc name)
let estr ~loc value = pexp_constant ~loc (Pconst_string (value, loc, None))
let indexed prefix index = prefix ^ string_of_int index

let indexed2 prefix first second =
  prefix ^ string_of_int first ^ "_" ^ string_of_int second

let apply ~loc function_ arguments =
  pexp_apply ~loc function_
    (List.map (fun argument -> (Nolabel, argument)) arguments)

let call ~loc name arguments = apply ~loc (evar ~loc name) arguments
let eapply ~loc name arguments = pexp_apply ~loc (evar ~loc name) arguments

let lambda ~loc patterns body =
  List.fold_right
    (fun pattern body -> pexp_fun ~loc Nolabel None pattern body)
    patterns body

let tuple_expression ~loc = function
  | [ expression ] -> expression
  | expressions -> pexp_tuple ~loc expressions

let tuple_pattern ~loc = function
  | [ pattern ] -> pattern
  | patterns -> ppat_tuple ~loc patterns

let sequence ~loc expressions tail =
  List.fold_right
    (fun expression rest -> pexp_sequence ~loc expression rest)
    expressions tail

let constructor_pattern ~loc constructor argument =
  ppat_construct ~loc (lident ~loc constructor.pcd_name.txt) argument

let field_binders ~prefix fields =
  List.mapi
    (fun index field ->
      let name = indexed prefix index in
      (field, name, pvar ~loc:field.pld_loc name))
    fields

let inline_record_pattern ~loc fields binders =
  ppat_record ~loc
    (List.map2
       (fun field (_, _, pattern) ->
         (Located.mk ~loc:field.pld_loc (Lident field.pld_name.txt), pattern))
       fields binders)
    Closed

let map_option map = function None -> None | Some value -> Some (map value)

let rec rewrite_expression rewrite expression =
  match rewrite expression with
  | Some replacement -> replacement
  | None ->
      let recur = rewrite_expression rewrite in
      let map_case case =
        {
          case with
          pc_guard = map_option recur case.pc_guard;
          pc_rhs = recur case.pc_rhs;
        }
      in
      let map_binding binding =
        { binding with pvb_expr = recur binding.pvb_expr }
      in
      let map_parameter parameter =
        match parameter.pparam_desc with
        | Pparam_val (label, default, pattern) ->
            {
              parameter with
              pparam_desc = Pparam_val (label, map_option recur default, pattern);
            }
        | Pparam_newtype _ -> parameter
      in
      let map_function_body = function
        | Pfunction_body body -> Pfunction_body (recur body)
        | Pfunction_cases (cases, loc, attributes) ->
            Pfunction_cases (List.map map_case cases, loc, attributes)
      in
      let desc =
        match expression.pexp_desc with
        | Pexp_let (flag, bindings, body) ->
            Pexp_let (flag, List.map map_binding bindings, recur body)
        | Pexp_function (parameters, constraint_, body) ->
            Pexp_function
              ( List.map map_parameter parameters,
                constraint_,
                map_function_body body )
        | Pexp_apply (function_, arguments) ->
            Pexp_apply
              ( recur function_,
                List.map (fun (label, value) -> (label, recur value)) arguments
              )
        | Pexp_match (value, cases) ->
            Pexp_match (recur value, List.map map_case cases)
        | Pexp_try (value, cases) ->
            Pexp_try (recur value, List.map map_case cases)
        | Pexp_tuple values -> Pexp_tuple (List.map recur values)
        | Pexp_construct (name, value) ->
            Pexp_construct (name, map_option recur value)
        | Pexp_variant (name, value) ->
            Pexp_variant (name, map_option recur value)
        | Pexp_record (fields, base) ->
            Pexp_record
              ( List.map (fun (name, value) -> (name, recur value)) fields,
                map_option recur base )
        | Pexp_field (value, field) -> Pexp_field (recur value, field)
        | Pexp_setfield (record, field, value) ->
            Pexp_setfield (recur record, field, recur value)
        | Pexp_array values -> Pexp_array (List.map recur values)
        | Pexp_ifthenelse (condition, yes, no) ->
            Pexp_ifthenelse (recur condition, recur yes, map_option recur no)
        | Pexp_sequence (first, second) ->
            Pexp_sequence (recur first, recur second)
        | Pexp_while (condition, body) ->
            Pexp_while (recur condition, recur body)
        | Pexp_for (pattern, first, last, direction, body) ->
            Pexp_for (pattern, recur first, recur last, direction, recur body)
        | Pexp_constraint (value, type_) -> Pexp_constraint (recur value, type_)
        | Pexp_coerce (value, source, target) ->
            Pexp_coerce (recur value, source, target)
        | Pexp_send (value, method_) -> Pexp_send (recur value, method_)
        | Pexp_setinstvar (name, value) -> Pexp_setinstvar (name, recur value)
        | Pexp_override fields ->
            Pexp_override
              (List.map (fun (name, value) -> (name, recur value)) fields)
        | Pexp_letmodule (name, module_, body) ->
            Pexp_letmodule
              (name, rewrite_module_expression rewrite module_, recur body)
        | Pexp_letexception (constructor, body) ->
            Pexp_letexception (constructor, recur body)
        | Pexp_assert value -> Pexp_assert (recur value)
        | Pexp_lazy value -> Pexp_lazy (recur value)
        | Pexp_poly (value, type_) -> Pexp_poly (recur value, type_)
        | Pexp_newtype (name, value) -> Pexp_newtype (name, recur value)
        | Pexp_open (open_, value) -> Pexp_open (open_, recur value)
        | Pexp_letop letop ->
            let map_binding_op binding =
              { binding with pbop_exp = recur binding.pbop_exp }
            in
            Pexp_letop
              {
                let_ = map_binding_op letop.let_;
                ands = List.map map_binding_op letop.ands;
                body = recur letop.body;
              }
        | Pexp_pack module_ ->
            Pexp_pack (rewrite_module_expression rewrite module_)
        | ( Pexp_ident _ | Pexp_constant _ | Pexp_new _ | Pexp_object _
          | Pexp_extension _ | Pexp_unreachable ) as desc ->
            desc
      in
      { expression with pexp_desc = desc }

and rewrite_module_expression rewrite module_ =
  let recur = rewrite_module_expression rewrite in
  let desc =
    match module_.pmod_desc with
    | Pmod_structure structure ->
        Pmod_structure (rewrite_structure rewrite structure)
    | Pmod_functor (parameter, body) -> Pmod_functor (parameter, recur body)
    | Pmod_apply (function_, argument) ->
        Pmod_apply (recur function_, recur argument)
    | Pmod_apply_unit function_ -> Pmod_apply_unit (recur function_)
    | Pmod_constraint (module_, type_) -> Pmod_constraint (recur module_, type_)
    | Pmod_unpack expression ->
        Pmod_unpack (rewrite_expression rewrite expression)
    | (Pmod_ident _ | Pmod_extension _) as desc -> desc
  in
  { module_ with pmod_desc = desc }

and rewrite_structure rewrite structure =
  List.map (rewrite_structure_item rewrite) structure

and rewrite_structure_item rewrite item =
  let desc =
    match item.pstr_desc with
    | Pstr_eval (expression, attributes) ->
        Pstr_eval (rewrite_expression rewrite expression, attributes)
    | Pstr_value (recursive, bindings) ->
        Pstr_value
          ( recursive,
            List.map
              (fun binding ->
                {
                  binding with
                  pvb_expr = rewrite_expression rewrite binding.pvb_expr;
                })
              bindings )
    | Pstr_module binding ->
        Pstr_module
          {
            binding with
            pmb_expr = rewrite_module_expression rewrite binding.pmb_expr;
          }
    | Pstr_recmodule bindings ->
        Pstr_recmodule
          (List.map
             (fun binding ->
               {
                 binding with
                 pmb_expr = rewrite_module_expression rewrite binding.pmb_expr;
               })
             bindings)
    | Pstr_include include_ ->
        Pstr_include
          {
            include_ with
            pincl_mod = rewrite_module_expression rewrite include_.pincl_mod;
          }
    | ( Pstr_primitive _ | Pstr_type _ | Pstr_typext _ | Pstr_exception _
      | Pstr_modtype _ | Pstr_open _ | Pstr_class _ | Pstr_class_type _
      | Pstr_attribute _ | Pstr_extension _ ) as desc ->
        desc
  in
  { item with pstr_desc = desc }

let iter_expression visit expression =
  ignore
    (rewrite_expression
       (fun expression ->
         visit expression;
         None)
       expression)

let rec iter_core_type visit type_ =
  visit type_;
  let recur = iter_core_type visit in
  match type_.ptyp_desc with
  | Ptyp_arrow (_, argument, result) ->
      recur argument;
      recur result
  | Ptyp_tuple types | Ptyp_constr (_, types) | Ptyp_class (_, types) ->
      List.iter recur types
  | Ptyp_object (fields, _) ->
      List.iter
        (fun field ->
          match field.pof_desc with
          | Otag (_, type_) | Oinherit type_ -> recur type_)
        fields
  | Ptyp_alias (type_, _) | Ptyp_poly (_, type_) | Ptyp_open (_, type_) ->
      recur type_
  | Ptyp_variant (rows, _, _) ->
      List.iter
        (fun row ->
          match row.prf_desc with
          | Rtag (_, _, types) -> List.iter recur types
          | Rinherit type_ -> recur type_)
        rows
  | Ptyp_package (_, constraints) ->
      List.iter (fun (_, type_) -> recur type_) constraints
  | Ptyp_any | Ptyp_var _ | Ptyp_extension _ -> ()

let iter_type_declaration visit declaration =
  List.iter
    (fun (parameter, _) -> iter_core_type visit parameter)
    declaration.ptype_params;
  List.iter
    (fun (left, right, _) ->
      iter_core_type visit left;
      iter_core_type visit right)
    declaration.ptype_cstrs;
  match declaration.ptype_kind with
  | Ptype_abstract ->
      Option.iter (iter_core_type visit) declaration.ptype_manifest
  | Ptype_record fields ->
      List.iter (fun field -> iter_core_type visit field.pld_type) fields
  | Ptype_variant constructors ->
      List.iter
        (fun constructor ->
          (match constructor.pcd_args with
          | Pcstr_tuple types -> List.iter (iter_core_type visit) types
          | Pcstr_record fields ->
              List.iter
                (fun field -> iter_core_type visit field.pld_type)
                fields);
          Option.iter (iter_core_type visit) constructor.pcd_res)
        constructors
  | Ptype_open -> ()
