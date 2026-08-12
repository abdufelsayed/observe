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

let for_ppx_call ~loc name arguments =
  call ~loc ("Observe.Type.For_ppx." ^ name) arguments

let rec expression_list ~loc = function
  | [] -> pexp_construct ~loc (lident ~loc "[]") None
  | head :: tail ->
      pexp_construct ~loc (lident ~loc "::")
        (Some (pexp_tuple ~loc [ head; expression_list ~loc tail ]))

let option_expression ~loc = function
  | None -> pexp_construct ~loc (lident ~loc "None") None
  | Some value -> pexp_construct ~loc (lident ~loc "Some") (Some value)

let internal_variant ~loc ~polymorphic name payload =
  pexp_apply ~loc
    (evar ~loc "Observe.Type.For_ppx.variant")
    [
      (Labelled "polymorphic", ebool ~loc polymorphic);
      (Nolabel, estr ~loc name);
      (Nolabel, option_expression ~loc payload);
    ]

let internal_record ~loc fields =
  let fields =
    List.map
      (fun (name, value) -> pexp_tuple ~loc [ estr ~loc name; value ])
      fields
  in
  for_ppx_call ~loc "record" [ expression_list ~loc fields ]

let last_identifier = function
  | Lident name | Ldot (_, name) -> Some name
  | Lapply _ -> None

let is_lazy_type = function
  | Lident "lazy_t" | Ldot (Lident "Lazy", "t") -> true
  | _ -> false

let rec present_sequence (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~presenters ~loc description values =
  let item = "__observe_item" in
  let present =
    presentation_of_core
      (module Engine : Ppx_repr_lib.Engine.S)
      ~library ~presenters description (evar ~loc item)
  in
  for_ppx_call ~loc "list_map"
    [ pexp_fun ~loc Nolabel None (pvar ~loc item) present; values ]

and presentation_of_core (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~presenters description value =
  let loc = description.ptyp_loc in
  match description.ptyp_desc with
  | Ptyp_var name -> for_ppx_call ~loc "present" [ evar ~loc name; value ]
  | Ptyp_constr ({ txt = Lident name; _ }, []) -> (
      match List.assoc_opt name presenters with
      | Some presenter -> call ~loc presenter [ value ]
      | None ->
          let description = Engine.expand_typ ?lib:library description in
          for_ppx_call ~loc "present" [ description; value ])
  | Ptyp_constr ({ txt; _ }, [ item ])
    when Option.equal String.equal (last_identifier txt) (Some "list") ->
      present_sequence
        (module Engine : Ppx_repr_lib.Engine.S)
        ~library ~presenters ~loc item value
  | Ptyp_constr ({ txt; _ }, [ item ])
    when Option.equal String.equal (last_identifier txt) (Some "array") ->
      let values = call ~loc "Array.to_list" [ value ] in
      present_sequence
        (module Engine : Ppx_repr_lib.Engine.S)
        ~library ~presenters ~loc item values
  | Ptyp_constr ({ txt; _ }, [ item ])
    when Option.equal String.equal (last_identifier txt) (Some "option") ->
      let some = "__observe_some" in
      pexp_match ~loc value
        [
          case
            ~lhs:(ppat_construct ~loc (lident ~loc "None") None)
            ~guard:None
            ~rhs:(for_ppx_call ~loc "option" [ option_expression ~loc None ]);
          case
            ~lhs:
              (ppat_construct ~loc (lident ~loc "Some") (Some (pvar ~loc some)))
            ~guard:None
            ~rhs:
              (for_ppx_call ~loc "option"
                 [
                   option_expression ~loc
                     (Some
                        (presentation_of_core
                           (module Engine : Ppx_repr_lib.Engine.S)
                           ~library ~presenters item (evar ~loc some)));
                 ]);
        ]
  | Ptyp_constr ({ txt; _ }, [ item ])
    when Option.equal String.equal (last_identifier txt) (Some "ref") ->
      presentation_of_core
        (module Engine : Ppx_repr_lib.Engine.S)
        ~library ~presenters item
        (call ~loc "(!)" [ value ])
  | Ptyp_constr ({ txt; _ }, [ item ]) when is_lazy_type txt ->
      presentation_of_core
        (module Engine : Ppx_repr_lib.Engine.S)
        ~library ~presenters item
        (call ~loc "Lazy.force" [ value ])
  | Ptyp_tuple descriptions ->
      let names =
        List.mapi
          (fun index _ -> Printf.sprintf "__observe_tuple_%d" index)
          descriptions
      in
      let items =
        List.map2
          (fun description name ->
            presentation_of_core
              (module Engine : Ppx_repr_lib.Engine.S)
              ~library ~presenters description (evar ~loc name))
          descriptions names
      in
      pexp_match ~loc value
        [
          case
            ~lhs:(ppat_tuple ~loc (List.map (pvar ~loc) names))
            ~guard:None
            ~rhs:(for_ppx_call ~loc "list" [ expression_list ~loc items ]);
        ]
  | Ptyp_variant (rows, Closed, _) ->
      presentation_of_polyvariant
        (module Engine : Ppx_repr_lib.Engine.S)
        ~library ~presenters ~loc rows value
  | _ ->
      let description = Engine.expand_typ ?lib:library description in
      for_ppx_call ~loc "present" [ description; value ]

and presentation_of_polyvariant (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~presenters ~loc rows value =
  let cases =
    List.map
      (fun row ->
        match row.prf_desc with
        | Rtag (label, _, []) ->
            case
              ~lhs:(ppat_variant ~loc:row.prf_loc label.txt None)
              ~guard:None
              ~rhs:
                (internal_variant ~loc:row.prf_loc ~polymorphic:true label.txt
                   None)
        | Rtag (label, _, descriptions) ->
            let names =
              List.mapi
                (fun index _ -> Printf.sprintf "__observe_poly_%d" index)
                descriptions
            in
            let pattern =
              match names with
              | [ name ] -> pvar ~loc:row.prf_loc name
              | names ->
                  ppat_tuple ~loc:row.prf_loc
                    (List.map (pvar ~loc:row.prf_loc) names)
            in
            let payload_type =
              match descriptions with
              | [ description ] -> description
              | descriptions -> ptyp_tuple ~loc:row.prf_loc descriptions
            in
            let payload_value =
              match names with
              | [ name ] -> evar ~loc:row.prf_loc name
              | names ->
                  pexp_tuple ~loc:row.prf_loc
                    (List.map (evar ~loc:row.prf_loc) names)
            in
            let payload =
              presentation_of_core
                (module Engine)
                ~library ~presenters payload_type payload_value
            in
            case
              ~lhs:(ppat_variant ~loc:row.prf_loc label.txt (Some pattern))
              ~guard:None
              ~rhs:
                (internal_variant ~loc:row.prf_loc ~polymorphic:true label.txt
                   (Some payload))
        | Rinherit _ ->
            inline_error ~loc:row.prf_loc
              "inherited polymorphic-variant rows are not supported")
      rows
  in
  pexp_match ~loc value cases

let presentation_of_fields (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~presenters fields value =
  let loc = value.pexp_loc in
  let fields =
    List.map
      (fun field ->
        let field_loc = field.pld_loc in
        let field_value =
          pexp_field ~loc:field_loc value
            (Located.mk ~loc:field_loc (Lident field.pld_name.txt))
        in
        ( field.pld_name.txt,
          presentation_of_core
            (module Engine)
            ~library ~presenters field.pld_type field_value ))
      fields
  in
  internal_record ~loc fields

let presentation_of_variant (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~presenters declaration constructors value =
  let cases =
    List.mapi
      (fun index constructor ->
        let loc = constructor.pcd_loc in
        let name = constructor.pcd_name.txt in
        match constructor.pcd_args with
        | Pcstr_tuple [] ->
            case
              ~lhs:(constructor_pattern ~loc constructor None)
              ~guard:None
              ~rhs:(internal_variant ~loc ~polymorphic:false name None)
        | Pcstr_tuple descriptions ->
            let names =
              List.mapi
                (fun field_index _ ->
                  Printf.sprintf "__observe_payload_%d_%d" index field_index)
                descriptions
            in
            let pattern = tuple_pattern ~loc (List.map (pvar ~loc) names) in
            let payload_type =
              match descriptions with
              | [ description ] -> description
              | descriptions -> ptyp_tuple ~loc descriptions
            in
            let payload_value =
              tuple_expression ~loc (List.map (evar ~loc) names)
            in
            let payload =
              presentation_of_core
                (module Engine)
                ~library ~presenters payload_type payload_value
            in
            case
              ~lhs:(constructor_pattern ~loc constructor (Some pattern))
              ~guard:None
              ~rhs:
                (internal_variant ~loc ~polymorphic:false name (Some payload))
        | Pcstr_record fields ->
            let binders = field_binders fields in
            let pattern = inline_record_pattern ~loc fields binders in
            let fields =
              List.map2
                (fun field (_, value_name, _) ->
                  ( field.pld_name.txt,
                    presentation_of_core
                      (module Engine)
                      ~library ~presenters field.pld_type
                      (evar ~loc:field.pld_loc value_name) ))
                fields binders
            in
            let payload = internal_record ~loc fields in
            case
              ~lhs:(constructor_pattern ~loc constructor (Some pattern))
              ~guard:None
              ~rhs:
                (internal_variant ~loc ~polymorphic:false name (Some payload)))
      constructors
  in
  pexp_match ~loc:declaration.ptype_loc value cases

let presentation_of_declaration (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~presenters declaration value =
  match declaration.ptype_kind with
  | Ptype_record fields ->
      presentation_of_fields (module Engine) ~library ~presenters fields value
  | Ptype_variant constructors ->
      presentation_of_variant
        (module Engine)
        ~library ~presenters declaration constructors value
  | Ptype_abstract -> (
      match declaration.ptype_manifest with
      | Some description ->
          presentation_of_core
            (module Engine)
            ~library ~presenters description value
      | None ->
          inline_error ~loc:declaration.ptype_loc
            "abstract types need a manifest")
  | Ptype_open ->
      inline_error ~loc:declaration.ptype_loc "open types are not supported"

let with_presentation (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~recursive declaration description =
  let loc = declaration.ptype_loc in
  let presenter = "__observe_present_" ^ declaration.ptype_name.txt in
  let presenters =
    if recursive then [ (declaration.ptype_name.txt, presenter) ] else []
  in
  let value = "__observe_value" in
  let body =
    presentation_of_declaration
      (module Engine)
      ~library ~presenters declaration (evar ~loc value)
  in
  let presenter_binding =
    value_binding ~loc ~pat:(pvar ~loc presenter)
      ~expr:(pexp_fun ~loc Nolabel None (pvar ~loc value) body)
  in
  let presenter_expression =
    pexp_let ~loc
      (if recursive && is_self_recursive declaration then Recursive
       else Nonrecursive)
      [ presenter_binding ] (evar ~loc presenter)
  in
  for_ppx_call ~loc "with_present" [ description; presenter_expression ]

let descriptor_parameters declaration =
  List.mapi
    (fun index (parameter, _) ->
      match parameter.ptyp_desc with
      | Ptyp_var name -> name
      | Ptyp_any -> Printf.sprintf "__observe_parameter_%d" index
      | _ -> assert false)
    declaration.ptype_params

let wrap_descriptor_parameters declaration wrap expression =
  let loc = declaration.ptype_loc in
  let parameters = descriptor_parameters declaration in
  let machine =
    match parameters with
    | [] -> expression
    | _ -> apply ~loc expression (List.map (evar ~loc) parameters)
  in
  lambda ~loc (List.map (pvar ~loc) parameters) (wrap machine)

let add_presentation (module Engine : Ppx_repr_lib.Engine.S) ~library ~recursive
    declaration items =
  match items with
  | ({ pstr_desc = Pstr_value (flag, [ binding ]); _ } as representation)
    :: rest ->
      let expression =
        wrap_descriptor_parameters declaration
          (with_presentation (module Engine) ~library ~recursive declaration)
          binding.pvb_expr
      in
      let binding = { binding with pvb_expr = expression } in
      { representation with pstr_desc = Pstr_value (flag, [ binding ]) } :: rest
  | _ ->
      inline_error ~loc:declaration.ptype_loc
        "unexpected descriptor expansion shape"

let add_group_presentation (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~recursive declarations items =
  match items with
  | ({ pstr_desc = Pstr_value (flag, [ binding ]); _ } as representation)
    :: rest ->
      let loc = binding.pvb_loc in
      let presenter_name declaration =
        "__observe_present_" ^ declaration.ptype_name.txt
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
          let presenters =
            List.map
              (fun declaration ->
                (declaration.ptype_name.txt, presenter_name declaration))
              declarations
          in
          let presenter_bindings =
            List.map
              (fun declaration ->
                let presenter = presenter_name declaration in
                let value = "__observe_value" in
                let body =
                  presentation_of_declaration
                    (module Engine)
                    ~library ~presenters declaration (evar ~loc value)
                in
                value_binding ~loc ~pat:(pvar ~loc presenter)
                  ~expr:(pexp_fun ~loc Nolabel None (pvar ~loc value) body))
              declarations
          in
          let values =
            List.map2
              (fun declaration machine ->
                for_ppx_call ~loc "with_present"
                  [ evar ~loc machine; evar ~loc (presenter_name declaration) ])
              declarations machine_names
            |> tuple_expression ~loc
          in
          pexp_let ~loc Recursive presenter_bindings values
        else
          List.map2
            (fun declaration machine ->
              let parameters = descriptor_parameters declaration in
              let machine =
                apply ~loc (evar ~loc machine) (List.map (evar ~loc) parameters)
              in
              let description =
                with_presentation
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
      let body =
        with_presentation
          (module Engine)
          ~library ~recursive:(rec_flag = Recursive) declaration body
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
        else
          match snd input with
          | [ declaration ] ->
              Engine.derive_str ~plugins ~name ~lib:library input
              |> add_presentation
                   (module Engine)
                   ~library
                   ~recursive:(fst input = Recursive)
                   declaration
          | _ :: _ as declarations ->
              Engine.derive_str ~plugins ~name ~lib:library input
              |> add_group_presentation
                   (module Engine)
                   ~library
                   ~recursive:(fst input = Recursive)
                   declarations
          | [] -> assert false )
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
