open Ppxlib
open Ast_builder.Default
open Generated_ast

let extension_error ~loc extension format =
  Location.raise_errorf ~loc ("[%%%s]: " ^^ format) extension

let is_body_form = function "text" | "untyped" | "typed" -> true | _ -> false

let expected_body ~loc extension =
  extension_error ~loc extension
    "expected [text ...], [untyped value], or [typed description value]"

let builder_field ~loc builder field =
  pexp_field ~loc (evar ~loc builder) (lident ~loc field)

let body_expression ~extension ~builder expression =
  let loc = expression.pexp_loc in
  match expression.pexp_desc with
  | Pexp_apply
      ({ pexp_desc = Pexp_ident { txt = Lident form; _ }; _ }, arguments)
    when is_body_form form ->
      pexp_apply ~loc (builder_field ~loc builder form) arguments
  | _ -> expected_body ~loc extension

let author_expression ~extension expression =
  let loc = expression.pexp_loc in
  let builder = gen_symbol ~prefix:"__observe_builder" () in
  pexp_fun ~loc Nolabel None (pvar ~loc builder)
    (body_expression ~extension ~builder expression)

let expand_level level ~loc ~path:_ expression =
  let extension = "observe." ^ level in
  eapply ~loc ("Observe.Logs." ^ level)
    [ (Nolabel, author_expression ~extension expression) ]

let expand_emit ~loc ~path:_ expression =
  let extension = "observe.emit" in
  match expression.pexp_desc with
  | Pexp_tuple [ level; body ] ->
      eapply ~loc "Observe.Logs.emit"
        [
          (Labelled "level", level); (Nolabel, author_expression ~extension body);
        ]
  | _ ->
      extension_error ~loc:expression.pexp_loc extension
        "expected [(level, text ...)], [(level, untyped value)], or [(level, \
         typed description value)]"

let expression_extension name expand =
  Extension.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand

let rules =
  List.map Context_free.Rule.extension
    [
      expression_extension "observe.debug" (expand_level "debug");
      expression_extension "observe.info" (expand_level "info");
      expression_extension "observe.warn" (expand_level "warn");
      expression_extension "observe.error" (expand_level "error");
      expression_extension "observe.emit" expand_emit;
    ]
