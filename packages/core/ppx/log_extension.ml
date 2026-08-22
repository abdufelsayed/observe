open Ppxlib
open Ast_builder.Default
open Generated_ast

let extension_error ~loc extension format =
  Location.raise_errorf ~loc ("[%%%s]: " ^^ format) extension

let is_body_form = function
  | "text" | "untyped" | "typed" | "error" -> true
  | _ -> false

let expected_body ~loc extension =
  extension_error ~loc extension
    "expected [text ...], [untyped { ... }], [typed schema value], or [error \
     interpretation value]"

let builder_field ~loc builder field =
  pexp_field ~loc (evar ~loc builder) (lident ~loc field)

let rec longident_last = function
  | Lident name | Ldot (_, name) -> name
  | Lapply (path, _) -> longident_last path

let qualify_description expression =
  let builtins =
    [
      "unit";
      "bool";
      "char";
      "int";
      "int32";
      "int63";
      "int64";
      "float";
      "string";
      "bytes";
      "boxed";
      "list";
      "array";
      "option";
      "pair";
      "triple";
      "quad";
      "result";
      "seq";
      "ref";
      "lazy_t";
      "queue";
      "stack";
      "hashtbl";
    ]
  in
  rewrite_expression
    (fun expression ->
      match expression.pexp_desc with
      | Pexp_ident { txt = Lident name; loc } when List.mem name builtins ->
          Some
            {
              expression with
              pexp_desc =
                Pexp_ident
                  { txt = Ldot (Ldot (Lident "Observe", "Type"), name); loc };
            }
      | _ -> None)
    expression

let described_value expression =
  let loc = expression.pexp_loc in
  match expression.pexp_desc with
  | Pexp_apply (description, arguments) -> (
      match List.rev arguments with
      | (Nolabel, value) :: reversed_description_arguments
        when List.for_all
               (fun (label, _) -> label = Nolabel)
               reversed_description_arguments ->
          let description =
            match List.rev reversed_description_arguments with
            | [] -> description
            | arguments -> pexp_apply ~loc description arguments
          in
          Some
            (eapply ~loc "Observe.Value.embed"
               [ (Nolabel, qualify_description description); (Nolabel, value) ])
      | _ -> None)
  | _ -> None

let anonymous_value ~extension expression =
  Value_extension.value_expression_with ~extension ~fallback:described_value
    expression

let rec typed_patch ~extension ~builder expression =
  let loc = expression.pexp_loc in
  match expression.pexp_desc with
  | Pexp_record (fields, None) ->
      let contributions =
        List.map
          (fun ({ txt = path; loc = field_loc }, value) ->
            let field = longident_last path in
            let value =
              match value.pexp_desc with
              | Pexp_record _ ->
                  let nested = gen_symbol ~prefix:"__observe_nested" () in
                  pexp_fun ~loc:field_loc Nolabel None
                    (pvar ~loc:field_loc nested)
                    (typed_patch ~extension ~builder:nested value)
              | _ -> value
            in
            pexp_apply ~loc:field_loc
              (builder_field ~loc:field_loc builder
                 ("__observe_field_" ^ field))
              [ (Nolabel, value) ])
          fields
      in
      pexp_apply ~loc
        (builder_field ~loc builder "__observe_combine")
        [ (Nolabel, elist ~loc contributions) ]
  | Pexp_record (_, Some _) ->
      extension_error ~loc extension "record updates are not sparse patches"
  | _ -> extension_error ~loc extension "expected a sparse record patch"

let body_expression ~extension ~builder expression =
  let loc = expression.pexp_loc in
  match expression.pexp_desc with
  | Pexp_apply
      ({ pexp_desc = Pexp_ident { txt = Lident form; _ }; _ }, arguments)
    when is_body_form form -> (
      match (form, arguments) with
      | "untyped", [ (Nolabel, ({ pexp_desc = Pexp_record _; _ } as object_)) ]
        ->
          pexp_apply ~loc
            (builder_field ~loc builder "value")
            [ (Nolabel, anonymous_value ~extension object_) ]
      | "untyped", _ ->
          extension_error ~loc extension
            "[untyped] requires one anonymous object; use the manual [m.value] \
             compatibility path for an existing Observe.Value.t"
      | _ -> pexp_apply ~loc (builder_field ~loc builder form) arguments)
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
      eapply ~loc "Observe.Logs.log"
        [
          (Labelled "level", level); (Nolabel, author_expression ~extension body);
        ]
  | _ ->
      extension_error ~loc:expression.pexp_loc extension
        "expected [(level, text ...)], [(level, untyped value)], or [(level, \
         typed description value)]"

let expand_set ~loc ~path:_ expression =
  let extension = "observe.set" in
  let expand handle body =
    let builder = gen_symbol ~prefix:"__observe_builder" () in
    let contribution =
      match body.pexp_desc with
      | Pexp_apply
          ( { pexp_desc = Pexp_ident { txt = Lident "untyped"; _ }; _ },
            [ (Nolabel, object_) ] ) ->
          eapply ~loc "Observe.Generated_runtime.open_value_patch"
            [ (Nolabel, anonymous_value ~extension object_) ]
      | Pexp_record _ ->
          pexp_apply ~loc
            (builder_field ~loc builder "typed")
            [ (Nolabel, typed_patch ~extension ~builder body) ]
      | Pexp_apply
          ({ pexp_desc = Pexp_ident { txt = Lident "error"; _ }; _ }, arguments)
        ->
          pexp_apply ~loc (builder_field ~loc builder "error") arguments
      | _ ->
          extension_error ~loc:body.pexp_loc extension
            "expected [handle untyped { ... }] or [handle { ... }]"
    in
    pexp_apply ~loc
      (evar ~loc "Observe.Logs.set")
      [
        (Nolabel, handle);
        (Nolabel, pexp_fun ~loc Nolabel None (pvar ~loc builder) contribution);
      ]
  in
  match expression.pexp_desc with
  | Pexp_apply
      ( handle,
        (Nolabel, { pexp_desc = Pexp_ident { txt = Lident "error"; _ }; _ })
        :: arguments ) ->
      expand handle (pexp_apply ~loc (evar ~loc "error") arguments)
  | Pexp_apply
      ( handle,
        [
          (Nolabel, { pexp_desc = Pexp_ident { txt = Lident "untyped"; _ }; _ });
          (Nolabel, object_);
        ] ) ->
      expand handle
        (pexp_apply ~loc (evar ~loc "untyped") [ (Nolabel, object_) ])
  | Pexp_apply (handle, [ (Nolabel, body) ]) -> expand handle body
  | _ ->
      extension_error ~loc:expression.pexp_loc extension
        "expected [handle untyped { ... }] or [handle { ... }]"

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
      expression_extension "observe.set" expand_set;
    ]
