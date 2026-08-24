open Ppxlib
open Ast_builder.Default
open Generated_ast

let for_ppx ~loc name arguments =
  pexp_apply ~loc (evar ~loc ("Observe.Ppx_runtime.Type." ^ name)) arguments

let field_binders fields =
  Generated_ast.field_binders ~prefix:"__observe_pretty_field_" fields

type context = {
  mutable next : int;
  mutable bindings_rev : value_binding list;
  recursive : (string * expression) list;
}

let create_context ?(recursive = []) () =
  { next = 0; bindings_rev = []; recursive }

let bindings context = List.rev context.bindings_rev

let replace_recursive context expression =
  rewrite_expression
    (fun expression ->
      match expression.pexp_desc with
      | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, _)
      | Pexp_ident { txt = Lident name; _ } ->
          List.assoc_opt name context.recursive
      | _ -> None)
    expression

let descriptor context (module Engine : Ppx_repr_lib.Engine.S) ~library
    description =
  match (Type_shape.has_custom_repr description, description.ptyp_desc) with
  | false, Ptyp_var name -> evar ~loc:description.ptyp_loc name
  | _ ->
      let loc = description.ptyp_loc in
      let name = "__observe_pretty_type_" ^ string_of_int context.next in
      context.next <- context.next + 1;
      context.bindings_rev <-
        value_binding ~loc ~pat:(pvar ~loc name)
          ~expr:
            (replace_recursive context
               (Type_shape.expand_descriptor
                  (module Engine)
                  ~library description))
        :: context.bindings_rev;
      evar ~loc name

let render_field context (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~renderer ~last name description value =
  let loc = description.ptyp_loc in
  for_ppx ~loc "field"
    [
      (Nolabel, descriptor context (module Engine) ~library description);
      (Nolabel, renderer);
      (Labelled "last", ebool ~loc last);
      (Labelled "name", estr ~loc name);
      (Nolabel, value);
    ]

let render_constructor context (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~renderer name description value =
  let loc = description.ptyp_loc in
  for_ppx ~loc "constructor"
    [
      (Nolabel, descriptor context (module Engine) ~library description);
      (Nolabel, renderer);
      (Labelled "last", ebool ~loc true);
      (Labelled "name", estr ~loc name);
      (Nolabel, value);
    ]

let with_children ~loc ~renderer ~placement body =
  let nested = "__observe_pretty_nested" in
  let start =
    for_ppx ~loc "start"
      [
        (Nolabel, renderer);
        (Nolabel, placement);
        (Labelled "scalar", ebool ~loc false);
      ]
  in
  pexp_let ~loc Nonrecursive
    [ value_binding ~loc ~pat:(pvar ~loc nested) ~expr:start ]
    (sequence ~loc body
       (for_ppx ~loc "finish"
          [ (Nolabel, renderer); (Nolabel, evar ~loc nested) ]))

let render_fields context (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~renderer fields values =
  List.mapi
    (fun index (field, value) ->
      render_field context
        (module Engine)
        ~library ~renderer
        ~last:(index = List.length fields - 1)
        field.pld_name.txt field.pld_type value)
    (List.combine fields values)

let record context (module Engine : Ppx_repr_lib.Engine.S) ~library fields
    renderer placement value =
  let loc = value.pexp_loc in
  let values =
    List.map
      (fun field ->
        pexp_field ~loc:field.pld_loc value
          (Located.mk ~loc:field.pld_loc (Lident field.pld_name.txt)))
      fields
  in
  with_children ~loc ~renderer ~placement
    (render_fields context (module Engine) ~library ~renderer fields values)

let payload_type ~loc = function
  | [ description ] -> description
  | descriptions -> ptyp_tuple ~loc descriptions

let variant context (module Engine : Ppx_repr_lib.Engine.S) ~library
    constructors renderer placement value =
  let cases =
    List.mapi
      (fun constructor_index constructor ->
        let loc = constructor.pcd_loc in
        let name = constructor.pcd_name.txt in
        match constructor.pcd_args with
        | Pcstr_tuple [] ->
            case
              ~lhs:(constructor_pattern ~loc constructor None)
              ~guard:None
              ~rhs:
                (for_ppx ~loc "variant"
                   [
                     (Nolabel, renderer);
                     (Nolabel, placement);
                     (Labelled "polymorphic", ebool ~loc false);
                     (Nolabel, estr ~loc name);
                   ])
        | Pcstr_tuple descriptions ->
            let names =
              List.mapi
                (fun index _ ->
                  "__observe_pretty_payload_"
                  ^ string_of_int constructor_index
                  ^ "_"
                  ^ string_of_int index)
                descriptions
            in
            let pattern = tuple_pattern ~loc (List.map (pvar ~loc) names) in
            let payload = tuple_expression ~loc (List.map (evar ~loc) names) in
            case
              ~lhs:(constructor_pattern ~loc constructor (Some pattern))
              ~guard:None
              ~rhs:
                (with_children ~loc ~renderer ~placement
                   [
                     render_constructor context
                       (module Engine)
                       ~library ~renderer name
                       (payload_type ~loc descriptions)
                       payload;
                   ])
        | Pcstr_record fields ->
            let binders = field_binders fields in
            let pattern = inline_record_pattern ~loc fields binders in
            let values =
              List.map (fun (_, field_name, _) -> evar ~loc field_name) binders
            in
            let constructor_nested = "__observe_pretty_constructor" in
            let constructor_start =
              for_ppx ~loc "constructor_start"
                [
                  (Nolabel, renderer);
                  (Labelled "last", ebool ~loc true);
                  (Labelled "name", estr ~loc name);
                  (Labelled "scalar", ebool ~loc false);
                ]
            in
            let fields =
              render_fields context
                (module Engine)
                ~library ~renderer fields values
            in
            let payload =
              pexp_let ~loc Nonrecursive
                [
                  value_binding ~loc
                    ~pat:(pvar ~loc constructor_nested)
                    ~expr:constructor_start;
                ]
                (sequence ~loc fields
                   (for_ppx ~loc "finish"
                      [
                        (Nolabel, renderer);
                        (Nolabel, evar ~loc constructor_nested);
                      ]))
            in
            case
              ~lhs:(constructor_pattern ~loc constructor (Some pattern))
              ~guard:None
              ~rhs:(with_children ~loc ~renderer ~placement [ payload ]))
      constructors
  in
  pexp_match ~loc:value.pexp_loc value cases

let polymorphic_variant context (module Engine : Ppx_repr_lib.Engine.S) ~library
    rows renderer placement value =
  let cases =
    List.mapi
      (fun row_index row ->
        let loc = row.prf_loc in
        match row.prf_desc with
        | Rtag (label, _, []) ->
            case
              ~lhs:(ppat_variant ~loc label.txt None)
              ~guard:None
              ~rhs:
                (for_ppx ~loc "variant"
                   [
                     (Nolabel, renderer);
                     (Nolabel, placement);
                     (Labelled "polymorphic", ebool ~loc true);
                     (Nolabel, estr ~loc label.txt);
                   ])
        | Rtag (label, _, descriptions) ->
            let names =
              List.mapi
                (fun index _ ->
                  "__observe_pretty_poly_"
                  ^ string_of_int row_index
                  ^ "_"
                  ^ string_of_int index)
                descriptions
            in
            let pattern = tuple_pattern ~loc (List.map (pvar ~loc) names) in
            let payload = tuple_expression ~loc (List.map (evar ~loc) names) in
            case
              ~lhs:(ppat_variant ~loc label.txt (Some pattern))
              ~guard:None
              ~rhs:
                (with_children ~loc ~renderer ~placement
                   [
                     render_constructor context
                       (module Engine)
                       ~library ~renderer ("`" ^ label.txt)
                       (payload_type ~loc descriptions)
                       payload;
                   ])
        | Rinherit _ ->
            Location.raise_errorf ~loc
              "[@@deriving observe]: inherited polymorphic-variant rows are \
               not supported")
      rows
  in
  pexp_match ~loc:value.pexp_loc value cases

let declaration context (module Engine : Ppx_repr_lib.Engine.S) ~library
    declaration renderer placement value =
  match declaration.ptype_kind with
  | Ptype_record fields ->
      record context (module Engine) ~library fields renderer placement value
  | Ptype_variant constructors ->
      variant context
        (module Engine)
        ~library constructors renderer placement value
  | Ptype_abstract -> (
      match declaration.ptype_manifest with
      | Some ({ ptyp_desc = Ptyp_variant (rows, Closed, _); _ } as description)
        ->
          let _ = description in
          polymorphic_variant context
            (module Engine)
            ~library rows renderer placement value
      | Some description ->
          for_ppx ~loc:description.ptyp_loc "render"
            [
              (Nolabel, descriptor context (module Engine) ~library description);
              (Nolabel, renderer);
              (Nolabel, placement);
              (Nolabel, value);
            ]
      | None ->
          Location.raise_errorf ~loc:declaration.ptype_loc
            "[@@deriving observe]: abstract types need a manifest")
  | Ptype_open ->
      Location.raise_errorf ~loc:declaration.ptype_loc
        "[@@deriving observe]: open types are not supported"

let variant_scalar constructors value =
  let cases =
    List.map
      (fun constructor ->
        let loc = constructor.pcd_loc in
        let argument =
          match constructor.pcd_args with
          | Pcstr_tuple [] -> None
          | Pcstr_tuple [ _ ] -> Some (ppat_any ~loc)
          | Pcstr_tuple descriptions ->
              Some
                (ppat_tuple ~loc
                   (List.map (fun _ -> ppat_any ~loc) descriptions))
          | Pcstr_record fields ->
              Some
                (ppat_record ~loc
                   (List.map
                      (fun field ->
                        ( Located.mk ~loc:field.pld_loc
                            (Lident field.pld_name.txt),
                          ppat_any ~loc:field.pld_loc ))
                      fields)
                   Closed)
        in
        case
          ~lhs:(constructor_pattern ~loc constructor argument)
          ~guard:None
          ~rhs:
            (ebool ~loc
               (match constructor.pcd_args with
               | Pcstr_tuple [] -> true
               | _ -> false)))
      constructors
  in
  pexp_match ~loc:value.pexp_loc value cases

let polymorphic_scalar rows value =
  let cases =
    List.map
      (fun row ->
        let loc = row.prf_loc in
        match row.prf_desc with
        | Rtag (label, _, []) ->
            case
              ~lhs:(ppat_variant ~loc label.txt None)
              ~guard:None ~rhs:(ebool ~loc true)
        | Rtag (label, _, descriptions) ->
            let payload =
              match descriptions with
              | [ _ ] -> ppat_any ~loc
              | _ ->
                  ppat_tuple ~loc
                    (List.map (fun _ -> ppat_any ~loc) descriptions)
            in
            case
              ~lhs:(ppat_variant ~loc label.txt (Some payload))
              ~guard:None ~rhs:(ebool ~loc false)
        | Rinherit _ ->
            Location.raise_errorf ~loc
              "[@@deriving observe]: inherited polymorphic-variant rows are \
               not supported")
      rows
  in
  pexp_match ~loc:value.pexp_loc value cases

let scalar context (module Engine : Ppx_repr_lib.Engine.S) ~library declaration
    value =
  match declaration.ptype_kind with
  | Ptype_record _ -> ebool ~loc:declaration.ptype_loc false
  | Ptype_variant constructors -> variant_scalar constructors value
  | Ptype_abstract -> (
      match declaration.ptype_manifest with
      | Some { ptyp_desc = Ptyp_variant (rows, Closed, _); _ } ->
          polymorphic_scalar rows value
      | Some description ->
          for_ppx ~loc:description.ptyp_loc "is_scalar"
            [
              (Nolabel, descriptor context (module Engine) ~library description);
              (Nolabel, value);
            ]
      | None ->
          Location.raise_errorf ~loc:declaration.ptype_loc
            "[@@deriving observe]: abstract types need a manifest")
  | Ptype_open ->
      Location.raise_errorf ~loc:declaration.ptype_loc
        "[@@deriving observe]: open types are not supported"
