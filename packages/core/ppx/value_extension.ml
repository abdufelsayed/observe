open Ppxlib
open Ast_builder.Default
open Generated_ast

let econstruct ~loc name argument =
  pexp_construct ~loc (lident ~loc name) argument

let rec elist ~loc = function
  | [] -> econstruct ~loc "[]" None
  | head :: tail ->
      econstruct ~loc "::" (Some (pexp_tuple ~loc [ head; elist ~loc tail ]))

let value_error ~loc format =
  Location.raise_errorf ~loc ("[%%observe.value]: " ^^ format)

let value_option ~loc value =
  eapply ~loc "Observe.Value.option" [ (Nolabel, value) ]

let rec value_expression expression =
  let loc = expression.pexp_loc in
  match expression.pexp_desc with
  | Pexp_constant (Pconst_integer (_, None)) ->
      eapply ~loc "Observe.Value.int" [ (Nolabel, expression) ]
  | Pexp_constant (Pconst_integer (_, Some _)) ->
      value_error ~loc
        "suffixed integer literals require [%%observe.value.embed \
         (description, value)]"
  | Pexp_constant (Pconst_float (_, None)) ->
      eapply ~loc "Observe.Value.float" [ (Nolabel, expression) ]
  | Pexp_constant (Pconst_float (_, Some _)) ->
      value_error ~loc
        "suffixed float literals require [%%observe.value.embed (description, \
         value)]"
  | Pexp_constant (Pconst_string _) ->
      eapply ~loc "Observe.Value.string" [ (Nolabel, expression) ]
  | Pexp_construct ({ txt = Lident "true"; _ }, None)
  | Pexp_construct ({ txt = Lident "false"; _ }, None) ->
      eapply ~loc "Observe.Value.bool" [ (Nolabel, expression) ]
  | Pexp_construct ({ txt = Lident "None"; _ }, None) ->
      value_option ~loc (econstruct ~loc "None" None)
  | Pexp_construct ({ txt = Lident "Some"; _ }, Some value) ->
      value_option ~loc (econstruct ~loc "Some" (Some (value_expression value)))
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) ->
      eapply ~loc "Observe.Value.list" [ (Nolabel, elist ~loc []) ]
  | Pexp_construct ({ txt = Lident "::"; _ }, Some tuple) ->
      value_list ~loc tuple
  | Pexp_record (fields, None) ->
      let fields =
        List.map
          (fun ({ txt; loc = label_loc }, value) ->
            match txt with
            | Lident label ->
                pexp_tuple ~loc:label_loc
                  [ estr ~loc:label_loc label; value_expression value ]
            | _ ->
                value_error ~loc:label_loc
                  "object keys must be unqualified identifiers")
          fields
      in
      eapply ~loc "Observe.Value.object_" [ (Nolabel, elist ~loc fields) ]
  | Pexp_record (_, Some _) ->
      value_error ~loc "record updates are not valid untyped objects"
  | Pexp_extension ({ txt = "observe.value.embed"; _ }, payload) ->
      value_embed ~loc payload
  | _ ->
      value_error ~loc
        "unsupported expression; use literals, objects, lists, options, or \
         [%%observe.value.embed (description, value)]"

and value_list ~loc tuple =
  match tuple.pexp_desc with
  | Pexp_tuple [ head; tail ] ->
      let rec collect values expression =
        match expression.pexp_desc with
        | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> List.rev values
        | Pexp_construct ({ txt = Lident "::"; _ }, Some tuple) -> (
            match tuple.pexp_desc with
            | Pexp_tuple [ head; tail ] ->
                collect (value_expression head :: values) tail
            | _ -> value_error ~loc:tuple.pexp_loc "malformed list literal")
        | _ ->
            value_error ~loc:expression.pexp_loc
              "list tails must be list literals"
      in
      eapply ~loc "Observe.Value.list"
        [ (Nolabel, elist ~loc (collect [ value_expression head ] tail)) ]
  | _ -> value_error ~loc:tuple.pexp_loc "malformed list literal"

and value_embed ~loc payload =
  match payload with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval ({ pexp_desc = Pexp_tuple [ description; value ]; _ }, []);
          _;
        };
      ] ->
      eapply ~loc "Observe.Value.embed"
        [ (Nolabel, description); (Nolabel, value) ]
  | _ ->
      value_error ~loc "expected [%%observe.value.embed (description, value)]"

let expand_value ~loc:_ ~path:_ expression = value_expression expression

let value_extension =
  Extension.declare "observe.value" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_value

let rules = [ Context_free.Rule.extension value_extension ]
