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
