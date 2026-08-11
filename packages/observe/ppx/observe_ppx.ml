open Ppxlib
open Ast_builder.Default

let lident ~loc name = Located.mk ~loc (Longident.parse name)
let evar ~loc name = pexp_ident ~loc (lident ~loc name)
let eapply ~loc name args = pexp_apply ~loc (evar ~loc name) args
let estr ~loc value = pexp_constant ~loc (Pconst_string (value, loc, None))

module Repr_attributes = struct
  let repr =
    Attribute.declare "observe.repr" Attribute.Context.Core_type
      Ast_pattern.(single_expr_payload __)
      Fun.id

  let nobuiltin =
    Attribute.declare "observe.nobuiltin" Attribute.Context.Core_type
      Ast_pattern.(pstr __')
      (fun payload ->
        match payload with
        | { txt = []; _ } -> ()
        | { loc; _ } ->
            Location.raise_errorf ~loc
              "[observe.nobuiltin] does not accept a payload")

  let all = Attribute.[ T repr; T nobuiltin ]
end

let with_repr_engine f ~loc ~path:_ =
  let (module Builder) = Ast_builder.make loc in
  f
    (module Ppx_repr_lib.Engine.Located (Repr_attributes) (Builder)
    : Ppx_repr_lib.Engine.S)

let repr_args () =
  let open Deriving.Args in
  Ppx_repr_lib.Meta_deriving.Args.[ arg "name" (estring __); arg "lib" __ ]

let default_repr_library = ref (Some "Observe.Type")

let repr_path library name =
  match library with Some library -> library ^ "." ^ name | None -> name

let repr_name_of_type_name = function "t" -> "t" | name -> name ^ "_t"

let has_inline_record declaration =
  match declaration.ptype_kind with
  | Ptype_variant constructors ->
      List.exists
        (fun constructor ->
          match constructor.pcd_args with Pcstr_record _ -> true | _ -> false)
        constructors
  | Ptype_abstract | Ptype_record _ | Ptype_open -> false

let is_self_recursive declaration =
  let type_name = declaration.ptype_name.txt in
  let visitor =
    object
      inherit [bool] Ast_traverse.fold as super

      method! core_type_desc description found =
        found
        ||
        match description with
        | Ptyp_constr ({ txt = Lident name; _ }, _)
          when String.equal name type_name ->
            true
        | _ -> super#core_type_desc description false
    end
  in
  visitor#type_declaration declaration false

let inline_error ~loc format =
  Location.raise_errorf ~loc ("[@@deriving observe]: " ^^ format)

let apply ~loc function_ arguments =
  pexp_apply ~loc function_
    (List.map (fun argument -> (Nolabel, argument)) arguments)

let call ~loc name arguments = apply ~loc (evar ~loc name) arguments

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

let field_binders fields =
  List.mapi
    (fun index field ->
      ( field,
        Printf.sprintf "__observe_field_%d" index,
        pvar ~loc:field.pld_loc (Printf.sprintf "__observe_field_%d" index) ))
    fields

let inline_payload_repr (module Engine : Ppx_repr_lib.Engine.S) ~library
    constructor fields =
  let loc = constructor.pcd_loc in
  let binders = field_binders fields in
  let values = List.map (fun (_, name, _) -> evar ~loc name) binders in
  let record =
    call ~loc
      (repr_path library "record")
      [
        estr ~loc constructor.pcd_name.txt;
        lambda ~loc
          (List.map (fun (_, _, p) -> p) binders)
          (tuple_expression ~loc values);
      ]
  in
  let record =
    List.fold_left
      (fun record (field, name, _) ->
        let field_loc = field.pld_loc in
        let payload_pattern =
          tuple_pattern ~loc:field_loc
            (List.map
               (fun (_, candidate, _) ->
                 if String.equal name candidate then
                   pvar ~loc:field_loc candidate
                 else ppat_any ~loc:field_loc)
               binders)
        in
        let descriptor = Engine.expand_typ ?lib:library field.pld_type in
        let field_repr =
          call ~loc:field_loc
            (repr_path library "field")
            [
              estr ~loc:field_loc field.pld_name.txt;
              descriptor;
              lambda ~loc:field_loc [ payload_pattern ]
                (evar ~loc:field_loc name);
            ]
        in
        call ~loc:field_loc (repr_path library "|+") [ record; field_repr ])
      record binders
  in
  call ~loc (repr_path library "sealr") [ record ]

let constructor_payload (module Engine : Ppx_repr_lib.Engine.S) ~library
    constructor =
  let loc = constructor.pcd_loc in
  match constructor.pcd_args with
  | Pcstr_tuple [] -> `Nullary
  | Pcstr_tuple types ->
      let payload_type =
        match types with [ type_ ] -> type_ | _ -> ptyp_tuple ~loc types
      in
      `Payload (Engine.expand_typ ?lib:library payload_type)
  | Pcstr_record fields ->
      `Payload (inline_payload_repr (module Engine) ~library constructor fields)

let constructor_value ~loc constructor argument =
  pexp_construct ~loc (lident ~loc constructor.pcd_name.txt) argument

let constructor_pattern ~loc constructor argument =
  ppat_construct ~loc (lident ~loc constructor.pcd_name.txt) argument

let inline_record_expression ~loc fields binders =
  pexp_record ~loc
    (List.map2
       (fun field (_, name, _) ->
         ( Located.mk ~loc:field.pld_loc (Lident field.pld_name.txt),
           evar ~loc:field.pld_loc name ))
       fields binders)
    None

let inline_record_pattern ~loc fields binders =
  ppat_record ~loc
    (List.map2
       (fun field (_, _, pattern) ->
         (Located.mk ~loc:field.pld_loc (Lident field.pld_name.txt), pattern))
       fields binders)
    Closed

type generated_case = {
  binder : pattern;
  matched : pattern;
  rhs : expression;
  repr : expression;
}

let inline_variant_expression (module Engine : Ppx_repr_lib.Engine.S) ~library
    declaration constructors =
  let loc = declaration.ptype_loc in
  let cases =
    List.mapi
      (fun index constructor ->
        let case_name = Printf.sprintf "__observe_case_%d" index in
        let case_pattern = pvar ~loc:constructor.pcd_loc case_name in
        match constructor.pcd_args with
        | Pcstr_tuple [] ->
            {
              binder = case_pattern;
              matched =
                constructor_pattern ~loc:constructor.pcd_loc constructor None;
              rhs = evar ~loc:constructor.pcd_loc case_name;
              repr =
                call ~loc:constructor.pcd_loc
                  (repr_path library "case0")
                  [
                    estr ~loc:constructor.pcd_loc constructor.pcd_name.txt;
                    constructor_value ~loc:constructor.pcd_loc constructor None;
                  ];
            }
        | Pcstr_tuple types ->
            let binders =
              List.mapi
                (fun field_index _ ->
                  Printf.sprintf "__observe_arg_%d_%d" index field_index)
                types
            in
            let patterns = List.map (pvar ~loc:constructor.pcd_loc) binders in
            let values = List.map (evar ~loc:constructor.pcd_loc) binders in
            let payload_pattern =
              tuple_pattern ~loc:constructor.pcd_loc patterns
            in
            let payload_value =
              tuple_expression ~loc:constructor.pcd_loc values
            in
            let descriptor =
              match
                constructor_payload (module Engine) ~library constructor
              with
              | `Payload descriptor -> descriptor
              | `Nullary -> assert false
            in
            let inject =
              lambda ~loc:constructor.pcd_loc [ payload_pattern ]
                (constructor_value ~loc:constructor.pcd_loc constructor
                   (Some payload_value))
            in
            {
              binder = case_pattern;
              matched =
                constructor_pattern ~loc:constructor.pcd_loc constructor
                  (Some payload_pattern);
              rhs = call ~loc:constructor.pcd_loc case_name [ payload_value ];
              repr =
                call ~loc:constructor.pcd_loc
                  (repr_path library "case1")
                  [
                    estr ~loc:constructor.pcd_loc constructor.pcd_name.txt;
                    descriptor;
                    inject;
                  ];
            }
        | Pcstr_record fields ->
            let binders = field_binders fields in
            let payload_pattern =
              tuple_pattern ~loc:constructor.pcd_loc
                (List.map (fun (_, _, p) -> p) binders)
            in
            let payload_value =
              tuple_expression ~loc:constructor.pcd_loc
                (List.map
                   (fun (_, name, _) -> evar ~loc:constructor.pcd_loc name)
                   binders)
            in
            let descriptor =
              inline_payload_repr (module Engine) ~library constructor fields
            in
            let inject =
              lambda ~loc:constructor.pcd_loc [ payload_pattern ]
                (constructor_value ~loc:constructor.pcd_loc constructor
                   (Some
                      (inline_record_expression ~loc:constructor.pcd_loc fields
                         binders)))
            in
            {
              binder = case_pattern;
              matched =
                constructor_pattern ~loc:constructor.pcd_loc constructor
                  (Some
                     (inline_record_pattern ~loc:constructor.pcd_loc fields
                        binders));
              rhs = call ~loc:constructor.pcd_loc case_name [ payload_value ];
              repr =
                call ~loc:constructor.pcd_loc
                  (repr_path library "case1")
                  [
                    estr ~loc:constructor.pcd_loc constructor.pcd_name.txt;
                    descriptor;
                    inject;
                  ];
            })
      constructors
  in
  let matcher_patterns, matcher_cases, repr_cases =
    List.fold_right
      (fun generated (patterns, matches, reprs) ->
        ( generated.binder :: patterns,
          case ~lhs:generated.matched ~guard:None ~rhs:generated.rhs :: matches,
          generated.repr :: reprs ))
      cases ([], [], [])
  in
  let matched_value = "__observe_value" in
  let matcher =
    lambda ~loc
      (matcher_patterns @ [ pvar ~loc matched_value ])
      (pexp_match ~loc (evar ~loc matched_value) matcher_cases)
  in
  let variant =
    call ~loc
      (repr_path library "variant")
      [ estr ~loc declaration.ptype_name.txt; matcher ]
  in
  let variant =
    List.fold_left
      (fun variant repr_case ->
        call ~loc (repr_path library "|~") [ variant; repr_case ])
      variant repr_cases
  in
  call ~loc (repr_path library "sealv") [ variant ]

let derive_inline_structure (module Engine : Ppx_repr_lib.Engine.S) ~plugins
    ~name ~library (rec_flag, declarations) =
  match (rec_flag, declarations) with
  | Recursive, [ declaration ] when is_self_recursive declaration ->
      inline_error ~loc:declaration.ptype_loc
        "recursive variants with inline-record constructors are not supported"
  | _, [ declaration ] when declaration.ptype_params <> [] ->
      inline_error ~loc:declaration.ptype_loc
        "parameterized variants with inline-record constructors are not \
         supported"
  | _, [ ({ ptype_kind = Ptype_variant constructors; _ } as declaration) ] ->
      List.iter
        (fun constructor ->
          if constructor.pcd_vars <> [] || Option.is_some constructor.pcd_res
          then
            inline_error ~loc:constructor.pcd_loc
              "GADT or existential inline-record variants are not supported")
        constructors;
      let repr_name =
        Option.value name
          ~default:(repr_name_of_type_name declaration.ptype_name.txt)
      in
      let body =
        inline_variant_expression
          (module Engine)
          ~library declaration constructors
      in
      let binding =
        value_binding ~loc:declaration.ptype_loc
          ~pat:(pvar ~loc:declaration.ptype_loc repr_name)
          ~expr:body
      in
      let representation =
        pstr_value ~loc:declaration.ptype_loc Nonrecursive [ binding ]
      in
      let derived =
        List.map
          (Ppx_repr_lib.Meta_deriving.Plugin.derive_str
             ~loc:declaration.ptype_loc ~type_name:declaration.ptype_name.txt
             ~params:[] ~expr:body)
          plugins
      in
      representation :: derived
  | _, declaration :: _ ->
      inline_error ~loc:declaration.ptype_loc
        "inline-record constructors must be derived in a single declaration"
  | _, [] -> assert false

let register_repr_deriver () =
  let supported_plugins = Ppx_repr_lib.Meta_deriving.Plugin.defaults in
  let library = !default_repr_library in
  let structure =
    Ppx_repr_lib.Meta_deriving.make_generator ~attributes:Repr_attributes.all
      ~supported_plugins ~args:(repr_args ())
      ( with_repr_engine @@ fun (module Engine) plugins input name lib ->
        let library = Option.fold lib ~none:library ~some:Engine.parse_lib in
        if List.exists has_inline_record (snd input) then
          derive_inline_structure (module Engine) ~plugins ~name ~library input
        else Engine.derive_str ~plugins ~name ~lib:library input )
  in
  let signature =
    Ppx_repr_lib.Meta_deriving.make_generator ~supported_plugins
      ~args:(repr_args ())
      ( with_repr_engine @@ fun (module Engine) plugins input name lib ->
        let library = Option.fold lib ~none:library ~some:Engine.parse_lib in
        Engine.derive_sig ~plugins ~name ~lib:library input )
  in
  Deriving.add ~str_type_decl:structure ~sig_type_decl:signature "observe"
  |> Deriving.ignore

let econstruct ~loc name argument =
  pexp_construct ~loc (lident ~loc name) argument

let punit ~loc = ppat_construct ~loc (lident ~loc "()") None

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
      value_error ~loc "record updates are not valid free-form objects"
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

let expand_value ~loc ~path:_ expression =
  pexp_fun ~loc Nolabel None (punit ~loc) (value_expression expression)

let value_extension =
  Extension.declare "observe.value" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_value

let () =
  register_repr_deriver ();
  Reserved_namespaces.reserve "observe";
  Driver.register_transformation "observe"
    ~rules:[ Context_free.Rule.extension value_extension ]
