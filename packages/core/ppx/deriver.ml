open Ppxlib
open Ast_builder.Default
open Generated_ast

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

let with_repr_engine f ~loc ~path =
  let (module Builder) = Ast_builder.make loc in
  f
    (module Ppx_repr_lib.Engine.Located (Repr_attributes) (Builder)
    : Ppx_repr_lib.Engine.S)
    ~path

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
  let found = ref false in
  iter_type_declaration
    (fun description ->
      match description.ptyp_desc with
      | Ptyp_constr ({ txt = Lident name; _ }, _)
        when String.equal name type_name ->
          found := true
      | _ -> ())
    declaration;
  !found

let inline_error ~loc format =
  Location.raise_errorf ~loc ("[@@deriving observe]: " ^^ format)

let field_binders fields =
  Generated_ast.field_binders ~prefix:"__observe_field_" fields

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
        let descriptor =
          Type_shape.expand_descriptor (module Engine) ~library field.pld_type
        in
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

let inline_record_expression ~loc fields binders =
  pexp_record ~loc
    (List.map2
       (fun field (_, name, _) ->
         ( Located.mk ~loc:field.pld_loc (Lident field.pld_name.txt),
           evar ~loc:field.pld_loc name ))
       fields binders)
    None

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
        let case_name = indexed "__observe_case_" index in
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
                  indexed2 "__observe_arg_" index field_index)
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

let for_ppx_call ~loc name arguments =
  call ~loc ("Observe.Generated_runtime." ^ name) arguments

let rendered ~loc ~scalar writer =
  pexp_ifthenelse ~loc scalar
    (pexp_construct ~loc
       (lident ~loc "Observe.Generated_runtime.Scalar")
       (Some
          (lambda ~loc
             [ pvar ~loc "__observe_renderer" ]
             (apply ~loc writer
                [
                  evar ~loc "__observe_renderer";
                  evar ~loc "Observe.Generated_runtime.inline";
                ]))))
    (Some
       (pexp_construct ~loc
          (lident ~loc "Observe.Generated_runtime.Node")
          (Some writer)))

let descriptor_parameters declaration =
  List.mapi
    (fun index (parameter, _) ->
      match parameter.ptyp_desc with
      | Ptyp_var name -> name
      | Ptyp_any -> indexed "__observe_parameter_" index
      | _ -> assert false)
    declaration.ptype_params

let recursive_freeze_descriptor declaration self expression =
  let recursive_name = repr_name_of_type_name declaration.ptype_name.txt in
  let parameters = descriptor_parameters declaration in
  rewrite_expression
    (fun expression ->
      match expression.pexp_desc with
      | Pexp_apply
          ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, arguments)
        when String.equal name recursive_name
             && List.length arguments = List.length parameters ->
          Some self
      | Pexp_ident { txt = Lident name; _ }
        when parameters = [] && String.equal name recursive_name ->
          Some self
      | _ -> None)
    expression

let variant_freezer (module Engine : Ppx_repr_lib.Engine.S) ~library ~recursive
    declaration =
  let loc = declaration.ptype_loc in
  let context = "__observe_freeze_context" in
  let depth = "__observe_freeze_depth" in
  let value = "__observe_freeze_value" in
  let self = "__observe_freeze_self" in
  let parameter_values =
    List.map (evar ~loc) (descriptor_parameters declaration)
  in
  let next_depth = call ~loc "Int.succ" [ evar ~loc depth ] in
  let descriptor ~apply_parameters expression =
    let expression =
      if apply_parameters then apply ~loc expression parameter_values
      else expression
    in
    if recursive then
      recursive_freeze_descriptor declaration (evar ~loc self) expression
    else expression
  in
  let freeze_payload ~loc ~apply_parameters description payload =
    let frozen = "__observe_frozen_payload" in
    let error = "__observe_freeze_error" in
    let attempted =
      pexp_apply ~loc
        (evar ~loc "Observe.Generated_runtime.freeze")
        [
          (Nolabel, descriptor ~apply_parameters description);
          (Nolabel, evar ~loc context);
          (Labelled "depth", next_depth);
          (Nolabel, payload);
        ]
    in
    let failed =
      case
        ~lhs:
          (ppat_alias ~loc
             (ppat_construct ~loc (lident ~loc "Error") (Some (ppat_any ~loc)))
             (Located.mk ~loc error))
        ~guard:None ~rhs:(evar ~loc error)
    in
    let succeeded body =
      case
        ~lhs:(ppat_construct ~loc (lident ~loc "Ok") (Some (pvar ~loc frozen)))
        ~guard:None
        ~rhs:(body (evar ~loc frozen))
    in
    (attempted, failed, succeeded)
  in
  let cases =
    match declaration.ptype_kind with
    | Ptype_variant constructors ->
        List.map
          (fun constructor ->
            let constructor_loc = constructor.pcd_loc in
            match constructor.pcd_args with
            | Pcstr_tuple [] ->
                case
                  ~lhs:
                    (constructor_pattern ~loc:constructor_loc constructor None)
                  ~guard:None
                  ~rhs:
                    (pexp_apply ~loc:constructor_loc
                       (evar ~loc:constructor_loc
                          "Observe.Generated_runtime.frozen_variant")
                       [
                         (Nolabel, evar ~loc:constructor_loc context);
                         (Labelled "depth", evar ~loc:constructor_loc depth);
                         (Nolabel, ebool ~loc:constructor_loc false);
                         ( Nolabel,
                           estr ~loc:constructor_loc constructor.pcd_name.txt );
                       ])
            | Pcstr_tuple types ->
                let binders =
                  List.mapi
                    (fun index _ -> indexed "__observe_freeze_field_" index)
                    types
                in
                let patterns = List.map (pvar ~loc:constructor_loc) binders in
                let payload_pattern =
                  tuple_pattern ~loc:constructor_loc patterns
                in
                let payload =
                  tuple_expression ~loc:constructor_loc
                    (List.map (evar ~loc:constructor_loc) binders)
                in
                let payload_type =
                  match types with
                  | [ type_ ] -> type_
                  | _ -> ptyp_tuple ~loc:constructor_loc types
                in
                let description = Engine.expand_typ ?lib:library payload_type in
                let attempted, failed, succeeded =
                  freeze_payload ~loc:constructor_loc ~apply_parameters:true
                    description payload
                in
                let variant frozen =
                  pexp_apply ~loc:constructor_loc
                    (evar ~loc:constructor_loc
                       "Observe.Generated_runtime.frozen_variant_payload")
                    [
                      (Nolabel, evar ~loc:constructor_loc context);
                      (Labelled "depth", evar ~loc:constructor_loc depth);
                      (Nolabel, ebool ~loc:constructor_loc false);
                      ( Nolabel,
                        estr ~loc:constructor_loc constructor.pcd_name.txt );
                      (Nolabel, frozen);
                    ]
                in
                case
                  ~lhs:
                    (constructor_pattern ~loc:constructor_loc constructor
                       (Some payload_pattern))
                  ~guard:None
                  ~rhs:
                    (pexp_match ~loc:constructor_loc attempted
                       [ failed; succeeded variant ])
            | Pcstr_record fields ->
                let binders = field_binders fields in
                let payload_pattern =
                  inline_record_pattern ~loc:constructor_loc fields binders
                in
                let payload =
                  tuple_expression ~loc:constructor_loc
                    (List.map
                       (fun (_, name, _) -> evar ~loc:constructor_loc name)
                       binders)
                in
                let description =
                  inline_payload_repr
                    (module Engine)
                    ~library constructor fields
                in
                let attempted, failed, succeeded =
                  freeze_payload ~loc:constructor_loc ~apply_parameters:false
                    description payload
                in
                let variant frozen =
                  pexp_apply ~loc:constructor_loc
                    (evar ~loc:constructor_loc
                       "Observe.Generated_runtime.frozen_variant_payload")
                    [
                      (Nolabel, evar ~loc:constructor_loc context);
                      (Labelled "depth", evar ~loc:constructor_loc depth);
                      (Nolabel, ebool ~loc:constructor_loc false);
                      ( Nolabel,
                        estr ~loc:constructor_loc constructor.pcd_name.txt );
                      (Nolabel, frozen);
                    ]
                in
                case
                  ~lhs:
                    (constructor_pattern ~loc:constructor_loc constructor
                       (Some payload_pattern))
                  ~guard:None
                  ~rhs:
                    (pexp_match ~loc:constructor_loc attempted
                       [ failed; succeeded variant ]))
          constructors
    | Ptype_abstract | Ptype_record _ | Ptype_open -> assert false
  in
  let body = pexp_match ~loc (evar ~loc value) cases in
  let body =
    pexp_fun ~loc Nolabel None (pvar ~loc context)
      (pexp_fun ~loc (Labelled "depth") None (pvar ~loc depth)
         (pexp_fun ~loc Nolabel None (pvar ~loc value) body))
  in
  if recursive then pexp_fun ~loc Nolabel None (pvar ~loc self) body else body

let polymorphic_variant_freezer (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~recursive declaration rows =
  let loc = declaration.ptype_loc in
  let context = "__observe_freeze_context" in
  let depth = "__observe_freeze_depth" in
  let value = "__observe_freeze_value" in
  let self = "__observe_freeze_self" in
  let descriptor description =
    let expression =
      Type_shape.expand_descriptor (module Engine) ~library description
    in
    if recursive then
      recursive_freeze_descriptor declaration (evar ~loc self) expression
    else expression
  in
  let cases =
    List.map
      (fun row ->
        let row_loc = row.prf_loc in
        match row.prf_desc with
        | Rtag (label, _, []) ->
            case
              ~lhs:(ppat_variant ~loc:row_loc label.txt None)
              ~guard:None
              ~rhs:
                (pexp_apply ~loc:row_loc
                   (evar ~loc:row_loc "Observe.Generated_runtime.frozen_variant")
                   [
                     (Nolabel, evar ~loc:row_loc context);
                     (Labelled "depth", evar ~loc:row_loc depth);
                     (Nolabel, ebool ~loc:row_loc true);
                     (Nolabel, estr ~loc:row_loc label.txt);
                   ])
        | Rtag (label, _, descriptions) ->
            let names =
              List.mapi
                (fun index _ -> indexed "__observe_freeze_field_" index)
                descriptions
            in
            let pattern =
              tuple_pattern ~loc:row_loc (List.map (pvar ~loc:row_loc) names)
            in
            let payload =
              tuple_expression ~loc:row_loc (List.map (evar ~loc:row_loc) names)
            in
            let payload_type =
              match descriptions with
              | [ description ] -> description
              | descriptions -> ptyp_tuple ~loc:row_loc descriptions
            in
            let frozen = "__observe_frozen_payload" in
            let error = "__observe_freeze_error" in
            let attempted =
              pexp_apply ~loc:row_loc
                (evar ~loc:row_loc "Observe.Generated_runtime.freeze")
                [
                  (Nolabel, descriptor payload_type);
                  (Nolabel, evar ~loc:row_loc context);
                  ( Labelled "depth",
                    call ~loc:row_loc "Int.succ" [ evar ~loc:row_loc depth ] );
                  (Nolabel, payload);
                ]
            in
            let failed =
              case
                ~lhs:
                  (ppat_alias ~loc:row_loc
                     (ppat_construct ~loc:row_loc
                        (lident ~loc:row_loc "Error")
                        (Some (ppat_any ~loc:row_loc)))
                     (Located.mk ~loc:row_loc error))
                ~guard:None ~rhs:(evar ~loc:row_loc error)
            in
            let succeeded =
              case
                ~lhs:
                  (ppat_construct ~loc:row_loc (lident ~loc:row_loc "Ok")
                     (Some (pvar ~loc:row_loc frozen)))
                ~guard:None
                ~rhs:
                  (pexp_apply ~loc:row_loc
                     (evar ~loc:row_loc
                        "Observe.Generated_runtime.frozen_variant_payload")
                     [
                       (Nolabel, evar ~loc:row_loc context);
                       (Labelled "depth", evar ~loc:row_loc depth);
                       (Nolabel, ebool ~loc:row_loc true);
                       (Nolabel, estr ~loc:row_loc label.txt);
                       (Nolabel, evar ~loc:row_loc frozen);
                     ])
            in
            case
              ~lhs:(ppat_variant ~loc:row_loc label.txt (Some pattern))
              ~guard:None
              ~rhs:(pexp_match ~loc:row_loc attempted [ failed; succeeded ])
        | Rinherit _ ->
            inline_error ~loc:row_loc
              "inherited polymorphic-variant rows are not supported")
      rows
  in
  let body =
    pexp_fun ~loc Nolabel None (pvar ~loc context)
      (pexp_fun ~loc (Labelled "depth") None (pvar ~loc depth)
         (pexp_fun ~loc Nolabel None (pvar ~loc value)
            (pexp_match ~loc (evar ~loc value) cases)))
  in
  if recursive then pexp_fun ~loc Nolabel None (pvar ~loc self) body else body

let with_generated (module Engine : Ppx_repr_lib.Engine.S) ~library ~recursive
    declaration description =
  let loc = declaration.ptype_loc in
  let encoder = "__observe_json_" ^ declaration.ptype_name.txt in
  let encoders =
    if recursive then [ (declaration.ptype_name.txt, encoder) ] else []
  in
  let value = "__observe_value" in
  let buffer = "__observe_buffer" in
  let json =
    Json.declaration
      (module Engine)
      ~library ~encoders declaration (evar ~loc buffer) (evar ~loc value)
  in
  let encoder_binding =
    value_binding ~loc ~pat:(pvar ~loc encoder)
      ~expr:(lambda ~loc [ pvar ~loc buffer; pvar ~loc value ] json)
  in
  let recursive_flag =
    if recursive && is_self_recursive declaration then Recursive
    else Nonrecursive
  in
  let description =
    for_ppx_call ~loc "with_json" [ description; evar ~loc encoder ]
  in
  let description =
    match declaration.ptype_kind with
    | Ptype_variant _ ->
        let freezer =
          variant_freezer (module Engine) ~library ~recursive declaration
        in
        if recursive then
          for_ppx_call ~loc "with_recursive_freeze" [ description; freezer ]
        else for_ppx_call ~loc "with_freeze" [ description; freezer ]
    | Ptype_abstract -> (
        match declaration.ptype_manifest with
        | Some { ptyp_desc = Ptyp_variant (rows, Closed, _); _ } ->
            let freezer =
              polymorphic_variant_freezer
                (module Engine)
                ~library ~recursive declaration rows
            in
            if recursive then
              for_ppx_call ~loc "with_recursive_freeze" [ description; freezer ]
            else for_ppx_call ~loc "with_freeze" [ description; freezer ]
        | Some _ | None -> description)
    | Ptype_record _ | Ptype_open -> description
  in
  let generated =
    let writer = "__observe_pretty_" ^ declaration.ptype_name.txt in
    let classifier = "__observe_scalar_" ^ declaration.ptype_name.txt in
    let renderer = "__observe_renderer" in
    let placement = "__observe_placement" in
    let value = "__observe_pretty_value" in
    let self = "__observe_pretty_self" in
    let pretty_context =
      if recursive then
        Pretty.create_context
          ~recursive:
            [
              (repr_name_of_type_name declaration.ptype_name.txt, evar ~loc self);
            ]
          ()
      else Pretty.create_context ()
    in
    let scalar_context = Pretty.create_context () in
    let pretty =
      Pretty.declaration pretty_context
        (module Engine)
        ~library declaration (evar ~loc renderer) (evar ~loc placement)
        (evar ~loc value)
    in
    let scalar =
      Pretty.scalar scalar_context
        (module Engine)
        ~library declaration (evar ~loc value)
    in
    let writer_body =
      lambda ~loc
        [ pvar ~loc renderer; pvar ~loc placement; pvar ~loc value ]
        pretty
    in
    let writer_body =
      match Pretty.bindings pretty_context with
      | [] -> writer_body
      | descriptions -> pexp_let ~loc Nonrecursive descriptions writer_body
    in
    let writer_body =
      if recursive then lambda ~loc [ pvar ~loc self ] writer_body
      else writer_body
    in
    let bindings =
      [
        value_binding ~loc ~pat:(pvar ~loc writer) ~expr:writer_body;
        value_binding ~loc ~pat:(pvar ~loc classifier)
          ~expr:(lambda ~loc [ pvar ~loc value ] scalar);
      ]
    in
    let plan_value = "__observe_plan_value" in
    let plan_self = "__observe_plan_self" in
    let writer_for value =
      let arguments =
        (if recursive then [ evar ~loc plan_self ] else [])
        @ [
            evar ~loc "__observe_renderer";
            evar ~loc "__observe_placement";
            value;
          ]
      in
      lambda ~loc
        [ pvar ~loc "__observe_renderer"; pvar ~loc "__observe_placement" ]
        (apply ~loc (evar ~loc writer) arguments)
    in
    let plan_body =
      rendered ~loc
        ~scalar:(apply ~loc (evar ~loc classifier) [ evar ~loc plan_value ])
        (writer_for (evar ~loc plan_value))
    in
    let plan = lambda ~loc [ pvar ~loc plan_value ] plan_body in
    let attach =
      if recursive then
        apply ~loc
          (evar ~loc "Observe.Generated_runtime.with_recursive_plan")
          [ description; lambda ~loc [ pvar ~loc plan_self ] plan ]
      else
        apply ~loc
          (evar ~loc "Observe.Generated_runtime.with_plan")
          [ description; plan ]
    in
    let generated = pexp_let ~loc Nonrecursive bindings attach in
    match Pretty.bindings scalar_context with
    | [] -> generated
    | descriptions -> pexp_let ~loc Nonrecursive descriptions generated
  in
  pexp_let ~loc recursive_flag [ encoder_binding ] generated

let patch_type_name = function "t" -> "patch" | name -> name ^ "_patch"

let patch_author_type_name = function
  | "t" -> "patch_author"
  | name -> name ^ "_patch_author"

let patch_builder_type_name = function
  | "t" -> "patch_builder"
  | name -> name ^ "_patch_builder"

let schema_value_name = function "t" -> "schema" | name -> name ^ "_schema"

let declared_identity ~path type_name =
  let rec source_marker index latest =
    if index + 3 > String.length path then latest
    else
      let marker =
        if
          index + 4 <= String.length path
          && String.equal (String.sub path index 4) ".mli"
        then Some (index, 4)
        else if String.equal (String.sub path index 3) ".ml" then Some (index, 3)
        else latest
      in
      source_marker (index + 1) marker
  in
  match source_marker 0 None with
  | None -> if String.equal path "" then type_name else path ^ "." ^ type_name
  | Some (marker, extension_length) ->
      let source = String.sub path 0 marker in
      let suffix_start = marker + extension_length in
      let suffix =
        String.sub path suffix_start (String.length path - suffix_start)
      in
      String.capitalize_ascii (Filename.basename source)
      ^ suffix
      ^ "."
      ^ type_name

let patch_value_name = function "t" -> "patch" | name -> name ^ "_patch"

let fragment_value_name = function
  | "t" -> "__observe_patch_fragment"
  | name -> "__observe_" ^ name ^ "_patch_fragment"

let author_fragment_value_name = function
  | "t" -> "__observe_patch_author_fragment"
  | name -> "__observe_" ^ name ^ "_patch_author_fragment"

let map_longident_last map = function
  | Lident name -> Lident (map name)
  | Ldot (path, name) -> Ldot (path, map name)
  | Lapply _ as path -> path

let type_path ~loc path arguments =
  ptyp_constr ~loc (Located.mk ~loc path) arguments

let declared_core_type declaration =
  let loc = declaration.ptype_loc in
  type_path ~loc (Lident declaration.ptype_name.txt)
    (List.map fst declaration.ptype_params)

let is_record declaration =
  match declaration.ptype_kind with Ptype_record _ -> true | _ -> false

let patch_core_type declaration =
  let loc = declaration.ptype_loc in
  if is_record declaration then
    type_path ~loc
      (Ldot (Ldot (Lident "Observe", "Schema"), "patch"))
      [ declared_core_type declaration ]
  else declared_core_type declaration

let schema_core_type declaration =
  let loc = declaration.ptype_loc in
  type_path ~loc
    (Ldot (Ldot (Lident "Observe", "Schema"), "t"))
    [
      declared_core_type declaration;
      type_path ~loc
        (Lident (patch_builder_type_name declaration.ptype_name.txt))
        (List.map fst declaration.ptype_params);
    ]

let description_core_type ~loc type_ =
  type_path ~loc (Ldot (Ldot (Lident "Observe", "Type"), "t")) [ type_ ]

let fragment_core_type ~loc =
  type_path ~loc
    (Ldot (Ldot (Lident "Observe", "Generated_runtime"), "fragment"))
    []

let patch_declaration declaration =
  let loc = declaration.ptype_loc in
  type_declaration ~loc
    ~name:(Located.mk ~loc (patch_type_name declaration.ptype_name.txt))
    ~params:declaration.ptype_params ~cstrs:[] ~kind:Ptype_abstract
    ~private_:Public
    ~manifest:(Some (patch_core_type declaration))

let builtin_type_names =
  [
    "unit";
    "bool";
    "char";
    "int";
    "int32";
    "int64";
    "float";
    "string";
    "bytes";
    "list";
    "array";
    "option";
    "result";
    "ref";
    "lazy_t";
    "seq";
    "queue";
    "stack";
    "hashtbl";
  ]

let rec longident_last = function
  | Lident name | Ldot (_, name) -> name
  | Lapply (path, _) -> longident_last path

let is_builtin_path path =
  List.mem (String.lowercase_ascii (longident_last path)) builtin_type_names

let local_type atomic_type_names = function
  | Lident name -> List.mem name atomic_type_names
  | Ldot _ | Lapply _ -> false

let atomic_patch_type ~atomic_type_names type_ path =
  Type_shape.has_custom_repr type_ || local_type atomic_type_names path

let patch_input_type ~atomic_type_names type_ =
  match type_.ptyp_desc with
  | Ptyp_constr ({ txt = path; _ }, arguments)
    when not
           (is_builtin_path path
           || atomic_patch_type ~atomic_type_names type_ path) ->
      {
        type_ with
        ptyp_desc =
          Ptyp_constr
            ( Located.mk ~loc:type_.ptyp_loc
                (map_longident_last patch_type_name path),
              arguments );
      }
  | _ -> type_

let patch_author_input_type ~atomic_type_names type_ =
  match type_.ptyp_desc with
  | Ptyp_constr ({ txt = path; _ }, arguments)
    when not
           (is_builtin_path path
           || atomic_patch_type ~atomic_type_names type_ path) ->
      {
        type_ with
        ptyp_desc =
          Ptyp_constr
            ( Located.mk ~loc:type_.ptyp_loc
                (map_longident_last patch_author_type_name path),
              arguments );
      }
  | _ -> type_

let patch_author_declaration declaration =
  let loc = declaration.ptype_loc in
  let manifest =
    if is_record declaration then
      ptyp_arrow ~loc Nolabel
        (type_path ~loc
           (Lident (patch_builder_type_name declaration.ptype_name.txt))
           (List.map fst declaration.ptype_params))
        (patch_core_type declaration)
    else declared_core_type declaration
  in
  type_declaration ~loc
    ~name:(Located.mk ~loc (patch_author_type_name declaration.ptype_name.txt))
    ~params:declaration.ptype_params ~cstrs:[] ~kind:Ptype_abstract
    ~private_:Public ~manifest:(Some manifest)

let arrow_chain ~loc arguments result =
  List.fold_right
    (fun (label, type_) result -> ptyp_arrow ~loc label type_ result)
    arguments result

let patch_builder_declaration ~private_ ~atomic_type_names declaration fields =
  let loc = declaration.ptype_loc in
  let patch = patch_core_type declaration in
  let arrow argument result = ptyp_arrow ~loc Nolabel argument result in
  let list type_ = type_path ~loc (Lident "list") [ type_ ] in
  let labels =
    let error_type = ptyp_var ~loc "error" in
    let interpretation =
      type_path ~loc
        (Ldot (Ldot (Lident "Observe", "Error"), "t"))
        [ error_type ]
    in
    let backtrace =
      type_path ~loc (Ldot (Lident "Printexc", "raw_backtrace")) []
    in
    let error_function =
      arrow_chain ~loc
        [
          (Nolabel, interpretation);
          (Optional "backtrace", backtrace);
          (Nolabel, error_type);
        ]
        patch
      |> ptyp_poly ~loc [ Located.mk ~loc "error" ]
    in
    [
      label_declaration ~loc ~name:(Located.mk ~loc "typed") ~mutable_:Immutable
        ~type_:(arrow patch patch);
      label_declaration ~loc ~name:(Located.mk ~loc "error") ~mutable_:Immutable
        ~type_:error_function;
      label_declaration ~loc
        ~name:(Located.mk ~loc "__observe_combine")
        ~mutable_:Immutable
        ~type_:(arrow (list patch) patch);
    ]
    @ List.map
        (fun field ->
          label_declaration ~loc:field.pld_loc
            ~name:
              (Located.mk ~loc:field.pld_loc
                 ("__observe_field_" ^ field.pld_name.txt))
            ~mutable_:Immutable
            ~type_:
              (arrow
                 (patch_author_input_type ~atomic_type_names field.pld_type)
                 patch))
        fields
  in
  type_declaration ~loc
    ~name:(Located.mk ~loc (patch_builder_type_name declaration.ptype_name.txt))
    ~params:declaration.ptype_params ~cstrs:[] ~kind:(Ptype_record labels)
    ~private_ ~manifest:None

let descriptor_argument_types declaration =
  List.map
    (fun (parameter, _) ->
      description_core_type ~loc:parameter.ptyp_loc parameter)
    declaration.ptype_params

let patch_signature_items ~atomic_type_names declaration =
  let loc = declaration.ptype_loc in
  let parameter_arguments =
    List.map
      (fun type_ -> (Nolabel, type_))
      (descriptor_argument_types declaration)
  in
  let fragment_type = fragment_core_type ~loc in
  let fragment_input =
    if is_record declaration then patch_core_type declaration
    else declared_core_type declaration
  in
  let fragment_description =
    value_description ~loc
      ~name:(Located.mk ~loc (fragment_value_name declaration.ptype_name.txt))
      ~type_:
        (arrow_chain ~loc
           (parameter_arguments @ [ (Nolabel, fragment_input) ])
           fragment_type)
      ~prim:[]
  in
  let author_fragment_description =
    value_description ~loc
      ~name:
        (Located.mk ~loc
           (author_fragment_value_name declaration.ptype_name.txt))
      ~type_:
        (arrow_chain ~loc
           (parameter_arguments
           @ [
               ( Nolabel,
                 type_path ~loc
                   (Lident (patch_author_type_name declaration.ptype_name.txt))
                   (List.map fst declaration.ptype_params) );
             ])
           fragment_type)
      ~prim:[]
  in
  let common type_items =
    type_items
    @ [
        psig_value ~loc fragment_description;
        psig_value ~loc author_fragment_description;
      ]
  in
  let atomic_common =
    common
      [
        psig_type ~loc Nonrecursive [ patch_declaration declaration ];
        psig_type ~loc Nonrecursive [ patch_author_declaration declaration ];
      ]
  in
  match declaration.ptype_kind with
  | Ptype_record fields ->
      let common =
        common
          [
            psig_type ~loc Nonrecursive [ patch_declaration declaration ];
            psig_type ~loc Nonrecursive
              [
                patch_builder_declaration ~private_:Private ~atomic_type_names
                  declaration fields;
              ];
            psig_type ~loc Nonrecursive [ patch_author_declaration declaration ];
          ]
      in
      let schema_description =
        value_description ~loc
          ~name:(Located.mk ~loc (schema_value_name declaration.ptype_name.txt))
          ~type_:
            (arrow_chain ~loc parameter_arguments
               (schema_core_type declaration))
          ~prim:[]
      in
      let patch_arguments =
        List.map
          (fun field ->
            ( Optional field.pld_name.txt,
              patch_input_type ~atomic_type_names field.pld_type ))
          fields
      in
      let patch_description =
        value_description ~loc
          ~name:(Located.mk ~loc (patch_value_name declaration.ptype_name.txt))
          ~type_:
            (arrow_chain ~loc
               (parameter_arguments
               @ patch_arguments
               @ [ (Nolabel, ptyp_constr ~loc (lident ~loc "unit") []) ])
               (patch_core_type declaration))
          ~prim:[]
      in
      common
      @ [
          psig_value ~loc schema_description; psig_value ~loc patch_description;
        ]
  | Ptype_abstract | Ptype_variant _ | Ptype_open -> atomic_common

let remap_local_descriptions local_descriptions expression =
  let names =
    List.map
      (fun (type_name, description_name) ->
        (repr_name_of_type_name type_name, description_name))
      local_descriptions
  in
  rewrite_expression
    (fun expression ->
      match expression.pexp_desc with
      | Pexp_ident ({ txt = Lident name; _ } as identifier) ->
          Option.map
            (fun replacement ->
              {
                expression with
                pexp_desc =
                  Pexp_ident { identifier with txt = Lident replacement };
              })
            (List.assoc_opt name names)
      | _ -> None)
    expression

let expanded_descriptor (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~local_descriptions type_ =
  Type_shape.expand_descriptor (module Engine) ~library type_
  |> remap_local_descriptions local_descriptions

let fragment_expression (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~atomic_type_names ~local_descriptions type_ value =
  let loc = type_.ptyp_loc in
  match type_.ptyp_desc with
  | Ptyp_constr ({ txt = path; _ }, arguments)
    when not
           (is_builtin_path path
           || atomic_patch_type ~atomic_type_names type_ path) ->
      let function_path = map_longident_last fragment_value_name path in
      let descriptions =
        List.map
          (expanded_descriptor (module Engine) ~library ~local_descriptions)
          arguments
      in
      pexp_apply ~loc
        (pexp_ident ~loc (Located.mk ~loc function_path))
        (List.map
           (fun argument -> (Nolabel, argument))
           (descriptions @ [ value ]))
  | _ ->
      let description =
        expanded_descriptor (module Engine) ~library ~local_descriptions type_
      in
      for_ppx_call ~loc "fragment" [ description; value ]

let author_fragment_expression (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~atomic_type_names ~local_descriptions type_ value =
  let loc = type_.ptyp_loc in
  match type_.ptyp_desc with
  | Ptyp_constr ({ txt = path; _ }, arguments)
    when not
           (is_builtin_path path
           || atomic_patch_type ~atomic_type_names type_ path) ->
      let function_path = map_longident_last author_fragment_value_name path in
      let descriptions =
        List.map
          (expanded_descriptor (module Engine) ~library ~local_descriptions)
          arguments
      in
      pexp_apply ~loc
        (pexp_ident ~loc (Located.mk ~loc function_path))
        (List.map
           (fun argument -> (Nolabel, argument))
           (descriptions @ [ value ]))
  | _ ->
      fragment_expression
        (module Engine)
        ~library ~atomic_type_names ~local_descriptions type_ value

let patch_structure_items (module Engine : Ppx_repr_lib.Engine.S) ~library ~path
    ~atomic_type_names ~local_descriptions ~description_name declaration =
  let loc = declaration.ptype_loc in
  let parameters = descriptor_parameters declaration in
  let parameter_patterns = List.map (pvar ~loc) parameters in
  let parameter_values = List.map (evar ~loc) parameters in
  let description = apply ~loc (evar ~loc description_name) parameter_values in
  let fragment_value = "__observe_patch_value" in
  let fragment_body =
    if is_record declaration then
      for_ppx_call ~loc "patch_fragment" [ evar ~loc fragment_value ]
    else for_ppx_call ~loc "fragment" [ description; evar ~loc fragment_value ]
  in
  let fragment_binding =
    value_binding ~loc
      ~pat:(pvar ~loc (fragment_value_name declaration.ptype_name.txt))
      ~expr:
        (lambda ~loc
           (parameter_patterns @ [ pvar ~loc fragment_value ])
           fragment_body)
  in
  match declaration.ptype_kind with
  | Ptype_record fields ->
      let schema_name = schema_value_name declaration.ptype_name.txt in
      let schema_name_parameter = "__observe_schema_name" in
      let named_patch field_fragment =
        for_ppx_call ~loc "named_record_patch_fields"
          [ evar ~loc schema_name_parameter; elist ~loc [ field_fragment ] ]
      in
      let builder_field field =
        let field_loc = field.pld_loc in
        let value_name = "__observe_builder_" ^ field.pld_name.txt in
        let fragment =
          author_fragment_expression
            (module Engine)
            ~library ~atomic_type_names ~local_descriptions field.pld_type
            (evar ~loc:field_loc value_name)
        in
        let field_fragment =
          for_ppx_call ~loc:field_loc "patch_field"
            [ estr ~loc:field_loc field.pld_name.txt; fragment ]
        in
        ( Located.mk ~loc:field_loc
            (Lident ("__observe_field_" ^ field.pld_name.txt)),
          pexp_fun ~loc:field_loc Nolabel None
            (pvar ~loc:field_loc value_name)
            (named_patch field_fragment) )
      in
      let typed_value = "__observe_typed_patch" in
      let patches_value = "__observe_patches" in
      let builder_record =
        let interpretation = "__observe_error_interpretation" in
        let backtrace = "__observe_error_backtrace" in
        let error_value = "__observe_error_value" in
        let error_fragment =
          pexp_apply ~loc
            (evar ~loc "Observe.Generated_runtime.error_fragment")
            [
              (Nolabel, evar ~loc interpretation);
              (Optional "backtrace", evar ~loc backtrace);
              (Nolabel, evar ~loc error_value);
            ]
        in
        let error_function =
          pexp_fun ~loc Nolabel None (pvar ~loc interpretation)
            (pexp_fun ~loc (Optional "backtrace") None (pvar ~loc backtrace)
               (pexp_fun ~loc Nolabel None (pvar ~loc error_value)
                  (for_ppx_call ~loc "named_error_patch"
                     [ evar ~loc schema_name_parameter; error_fragment ])))
        in
        pexp_record ~loc
          (( Located.mk ~loc (Lident "typed"),
             pexp_fun ~loc Nolabel None (pvar ~loc typed_value)
               (evar ~loc typed_value) )
          :: (Located.mk ~loc (Lident "error"), error_function)
          :: ( Located.mk ~loc (Lident "__observe_combine"),
               pexp_fun ~loc Nolabel None (pvar ~loc patches_value)
                 (for_ppx_call ~loc "combine_named_patches"
                    [ evar ~loc schema_name_parameter; evar ~loc patches_value ])
             )
          :: List.map builder_field fields)
          None
      in
      let builder_factory =
        pexp_fun ~loc Nolabel None
          (pvar ~loc schema_name_parameter)
          builder_record
      in
      let schema_expression =
        let identity = declared_identity ~path declaration.ptype_name.txt in
        pexp_apply ~loc
          (evar ~loc "Observe.Generated_runtime.record_schema")
          [
            (Labelled "name", estr ~loc identity);
            (Labelled "builder", builder_factory);
            (Nolabel, description);
          ]
      in
      let schema_binding =
        value_binding ~loc ~pat:(pvar ~loc schema_name)
          ~expr:(lambda ~loc parameter_patterns schema_expression)
      in
      let present_field_expression field value_name =
        let field_loc = field.pld_loc in
        let fragment =
          fragment_expression
            (module Engine)
            ~library ~atomic_type_names ~local_descriptions field.pld_type
            (evar ~loc:field_loc value_name)
        in
        for_ppx_call ~loc:field_loc "patch_field"
          [ estr ~loc:field_loc field.pld_name.txt; fragment ]
      in
      let indexed_fields =
        List.mapi (fun index field -> (index, field)) fields
      in
      let fields_expression =
        List.fold_right
          (fun (index, field) rest ->
            let field_loc = field.pld_loc in
            let option_name = indexed "__observe_patch_option_" index in
            let value_name = indexed "__observe_patch_value_" index in
            let present = present_field_expression field value_name in
            let cons =
              pexp_construct ~loc:field_loc
                (lident ~loc:field_loc "::")
                (Some (pexp_tuple ~loc:field_loc [ present; rest ]))
            in
            pexp_match ~loc:field_loc
              (evar ~loc:field_loc option_name)
              [
                case
                  ~lhs:
                    (ppat_construct ~loc:field_loc
                       (lident ~loc:field_loc "None")
                       None)
                  ~guard:None ~rhs:rest;
                case
                  ~lhs:
                    (ppat_construct ~loc:field_loc
                       (lident ~loc:field_loc "Some")
                       (Some (pvar ~loc:field_loc value_name)))
                  ~guard:None ~rhs:cons;
              ])
          indexed_fields (elist ~loc [])
      in
      let patch_body =
        for_ppx_call ~loc "record_patch_fields"
          [
            apply ~loc (evar ~loc schema_name) parameter_values;
            fields_expression;
          ]
      in
      let patch_body = pexp_fun ~loc Nolabel None (punit ~loc) patch_body in
      let patch_body =
        List.fold_right
          (fun (index, field) body ->
            let field_loc = field.pld_loc in
            pexp_fun ~loc:field_loc (Optional field.pld_name.txt) None
              (pvar ~loc:field_loc (indexed "__observe_patch_option_" index))
              body)
          indexed_fields patch_body
      in
      let patch_body = lambda ~loc parameter_patterns patch_body in
      let patch_binding =
        value_binding ~loc
          ~pat:(pvar ~loc (patch_value_name declaration.ptype_name.txt))
          ~expr:patch_body
      in
      let author_value = "__observe_patch_author" in
      let authored_patch =
        pexp_apply ~loc (evar ~loc author_value)
          [
            ( Nolabel,
              for_ppx_call ~loc "schema_builder"
                [ apply ~loc (evar ~loc schema_name) parameter_values ] );
          ]
      in
      let author_fragment_binding =
        value_binding ~loc
          ~pat:
            (pvar ~loc (author_fragment_value_name declaration.ptype_name.txt))
          ~expr:
            (lambda ~loc
               (parameter_patterns @ [ pvar ~loc author_value ])
               (for_ppx_call ~loc "patch_fragment" [ authored_patch ]))
      in
      [
        pstr_type ~loc Nonrecursive [ patch_declaration declaration ];
        pstr_type ~loc Nonrecursive
          [
            patch_builder_declaration ~private_:Public ~atomic_type_names
              declaration fields;
          ];
        pstr_type ~loc Nonrecursive [ patch_author_declaration declaration ];
        pstr_value ~loc Nonrecursive [ fragment_binding ];
      ]
      @ [
          pstr_value ~loc Nonrecursive [ schema_binding ];
          pstr_value ~loc Nonrecursive [ author_fragment_binding ];
          pstr_value ~loc Nonrecursive [ patch_binding ];
        ]
  | Ptype_abstract | Ptype_variant _ | Ptype_open ->
      let author_fragment_binding =
        value_binding ~loc
          ~pat:
            (pvar ~loc (author_fragment_value_name declaration.ptype_name.txt))
          ~expr:
            (lambda ~loc
               (parameter_patterns @ [ pvar ~loc fragment_value ])
               fragment_body)
      in
      [
        pstr_type ~loc Nonrecursive [ patch_declaration declaration ];
        pstr_type ~loc Nonrecursive [ patch_author_declaration declaration ];
        pstr_value ~loc Nonrecursive [ fragment_binding ];
        pstr_value ~loc Nonrecursive [ author_fragment_binding ];
      ]

let wrap_descriptor_parameters declaration wrap expression =
  let loc = declaration.ptype_loc in
  let parameters = descriptor_parameters declaration in
  let machine =
    match parameters with
    | [] -> expression
    | _ -> apply ~loc expression (List.map (evar ~loc) parameters)
  in
  lambda ~loc (List.map (pvar ~loc) parameters) (wrap machine)

let normalize_recursive_parameter_application declaration expression =
  let recursive_name = repr_name_of_type_name declaration.ptype_name.txt in
  let parameters = descriptor_parameters declaration in
  let is_parameter_argument parameter = function
    | { pexp_desc = Pexp_ident { txt = Lident name; _ }; _ } ->
        String.equal parameter name
    | _ -> false
  in
  rewrite_expression
    (fun expression ->
      match expression.pexp_desc with
      | Pexp_apply
          ( ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ } as self),
            arguments )
        when String.equal name recursive_name
             && List.length arguments = List.length parameters
             && List.for_all2
                  (fun parameter (label, argument) ->
                    label = Nolabel && is_parameter_argument parameter argument)
                  parameters arguments ->
          Some self
      | _ -> None)
    expression

let add_generated (module Engine : Ppx_repr_lib.Engine.S) ~library ~recursive
    declaration items =
  match items with
  | ({ pstr_desc = Pstr_value (flag, [ binding ]); _ } as representation)
    :: rest ->
      let machine =
        if recursive then
          normalize_recursive_parameter_application declaration binding.pvb_expr
        else binding.pvb_expr
      in
      let expression =
        wrap_descriptor_parameters declaration
          (with_generated (module Engine) ~library ~recursive declaration)
          machine
      in
      let binding = { binding with pvb_expr = expression } in
      { representation with pstr_desc = Pstr_value (flag, [ binding ]) } :: rest
  | _ ->
      inline_error ~loc:declaration.ptype_loc
        "unexpected descriptor expansion shape"

let add_group_generated (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~recursive declarations items =
  match items with
  | ({ pstr_desc = Pstr_value (flag, [ binding ]); _ } as representation)
    :: rest ->
      let loc = binding.pvb_loc in
      let encoder_name declaration =
        "__observe_json_" ^ declaration.ptype_name.txt
      in
      let machine_names =
        List.map
          (fun declaration -> "__observe_machine_" ^ declaration.ptype_name.txt)
          declarations
      in
      let machine_binding =
        value_binding ~loc
          ~pat:(tuple_pattern ~loc (List.map (pvar ~loc) machine_names))
          ~expr:binding.pvb_expr
      in
      let wrapped =
        if recursive then
          let encoders =
            List.map
              (fun declaration ->
                (declaration.ptype_name.txt, encoder_name declaration))
              declarations
          in
          let encoder_bindings =
            List.map
              (fun declaration ->
                let encoder = encoder_name declaration in
                let buffer = "__observe_buffer" in
                let value = "__observe_value" in
                let body =
                  Json.declaration
                    (module Engine)
                    ~library ~encoders declaration (evar ~loc buffer)
                    (evar ~loc value)
                in
                value_binding ~loc ~pat:(pvar ~loc encoder)
                  ~expr:(lambda ~loc [ pvar ~loc buffer; pvar ~loc value ] body))
              declarations
          in
          let values =
            List.map2
              (fun declaration machine ->
                for_ppx_call ~loc "with_json"
                  [ evar ~loc machine; evar ~loc (encoder_name declaration) ])
              declarations machine_names
            |> tuple_expression ~loc
          in
          pexp_let ~loc Recursive encoder_bindings values
        else
          List.map2
            (fun declaration machine ->
              let parameters = descriptor_parameters declaration in
              let machine =
                apply ~loc (evar ~loc machine) (List.map (evar ~loc) parameters)
              in
              let description =
                with_generated
                  (module Engine)
                  ~library ~recursive:false declaration machine
              in
              lambda ~loc (List.map (pvar ~loc) parameters) description)
            declarations machine_names
          |> tuple_expression ~loc
      in
      let expression = pexp_let ~loc Nonrecursive [ machine_binding ] wrapped in
      let binding = { binding with pvb_expr = expression } in
      { representation with pstr_desc = Pstr_value (flag, [ binding ]) } :: rest
  | _ ->
      inline_error ~loc:(List.hd declarations).ptype_loc
        "unexpected descriptor group expansion shape"

let derive_inline_structure (module Engine : Ppx_repr_lib.Engine.S) ~plugins
    ~name ~library (rec_flag, declarations) =
  match (rec_flag, declarations) with
  | Recursive, [ declaration ] when is_self_recursive declaration ->
      inline_error ~loc:declaration.ptype_loc
        "recursive variants with inline-record constructors are not supported"
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
      let generated =
        inline_variant_expression
          (module Engine)
          ~library declaration constructors
      in
      let generated =
        with_generated
          (module Engine)
          ~library
          ~recursive:(rec_flag = Recursive && is_self_recursive declaration)
          declaration generated
      in
      let parameters = descriptor_parameters declaration in
      let body =
        lambda ~loc:declaration.ptype_loc
          (List.map (pvar ~loc:declaration.ptype_loc) parameters)
          generated
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
             ~params:parameters ~expr:generated)
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
      ( with_repr_engine @@ fun (module Engine) ~path plugins input name lib ->
        Type_shape.validate_group input;
        let library = Option.fold lib ~none:library ~some:Engine.parse_lib in
        let declarations = snd input in
        let atomic_type_names =
          List.map (fun declaration -> declaration.ptype_name.txt) declarations
        in
        let local_descriptions =
          match (name, declarations) with
          | Some description_name, [ declaration ] ->
              [ (declaration.ptype_name.txt, description_name) ]
          | None, _ | Some _, [] ->
              List.map
                (fun declaration ->
                  ( declaration.ptype_name.txt,
                    repr_name_of_type_name declaration.ptype_name.txt ))
                declarations
          | Some _, declaration :: _ :: _ ->
              inline_error ~loc:declaration.ptype_loc
                "a custom description name requires one type declaration"
        in
        let descriptions =
          if List.exists has_inline_record declarations then
            derive_inline_structure
              (module Engine)
              ~plugins ~name ~library input
          else
            match declarations with
            | [ declaration ] ->
                Engine.derive_str ~plugins ~name ~lib:library input
                |> add_generated
                     (module Engine)
                     ~library
                     ~recursive:
                       (fst input = Recursive && is_self_recursive declaration)
                     declaration
            | _ :: _ as declarations ->
                Engine.derive_str ~plugins ~name ~lib:library input
                |> add_group_generated
                     (module Engine)
                     ~library
                     ~recursive:(fst input = Recursive)
                     declarations
            | [] -> assert false
        in
        descriptions
        @ List.concat_map
            (fun declaration ->
              patch_structure_items
                (module Engine)
                ~library ~path ~atomic_type_names ~local_descriptions
                ~description_name:
                  (List.assoc declaration.ptype_name.txt local_descriptions)
                declaration)
            declarations )
  in
  let signature =
    Ppx_repr_lib.Meta_deriving.make_generator ~supported_plugins
      ~args:(repr_args ())
      ( with_repr_engine @@ fun (module Engine) ~path:_ plugins input name lib ->
        Type_shape.validate_group input;
        let library = Option.fold lib ~none:library ~some:Engine.parse_lib in
        let declarations = snd input in
        let atomic_type_names =
          List.map (fun declaration -> declaration.ptype_name.txt) declarations
        in
        Engine.derive_sig ~plugins ~name ~lib:library input
        @ List.concat_map
            (patch_signature_items ~atomic_type_names)
            declarations )
  in
  Deriving.add ~str_type_decl:structure ~sig_type_decl:signature "observe"
  |> Deriving.ignore

let register = register_repr_deriver
