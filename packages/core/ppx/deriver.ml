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

let descriptor_parameters declaration =
  List.mapi
    (fun index (parameter, _) ->
      match parameter.ptyp_desc with
      | Ptyp_var name -> name
      | Ptyp_any -> indexed "__observe_parameter_" index
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

let normalize_recursive_parameter_application declaration expression =
  let recursive_name = repr_name_of_type_name declaration.ptype_name.txt in
  let parameters = descriptor_parameters declaration in
  let is_parameter_argument parameter = function
    | { pexp_desc = Pexp_ident { txt = Lident name; _ }; _ } ->
        String.equal parameter name
    | _ -> false
  in
  let mapper =
    object
      inherit Ast_traverse.map as super

      method! expression expression =
        match expression.pexp_desc with
        | Pexp_apply
            ( ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ } as self),
              arguments )
          when String.equal name recursive_name
               && List.length arguments = List.length parameters
               && List.for_all2
                    (fun parameter (label, argument) ->
                      label = Nolabel
                      && is_parameter_argument parameter argument)
                    parameters arguments ->
            self
        | _ -> super#expression expression
    end
  in
  mapper#expression expression

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
      ( with_repr_engine @@ fun (module Engine) plugins input name lib ->
        Type_shape.validate_group input;
        let library = Option.fold lib ~none:library ~some:Engine.parse_lib in
        if List.exists has_inline_record (snd input) then
          derive_inline_structure (module Engine) ~plugins ~name ~library input
        else
          match snd input with
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
          | [] -> assert false )
  in
  let signature =
    Ppx_repr_lib.Meta_deriving.make_generator ~supported_plugins
      ~args:(repr_args ())
      ( with_repr_engine @@ fun (module Engine) plugins input name lib ->
        Type_shape.validate_group input;
        let library = Option.fold lib ~none:library ~some:Engine.parse_lib in
        Engine.derive_sig ~plugins ~name ~lib:library input )
  in
  Deriving.add ~str_type_decl:structure ~sig_type_decl:signature "observe"
  |> Deriving.ignore

let register = register_repr_deriver
