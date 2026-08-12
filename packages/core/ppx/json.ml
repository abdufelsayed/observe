open Ppxlib
open Ast_builder.Default
open Generated_ast

let field_binders fields =
  Generated_ast.field_binders ~prefix:"__observe_field_" fields

let inline_error ~loc format =
  Location.raise_errorf ~loc ("[@@deriving observe]: " ^^ format)

let for_ppx_call ~loc name arguments =
  call ~loc ("Observe.Generated_runtime." ^ name) arguments

let buffer_add_char ~loc buffer character =
  call ~loc "Buffer.add_char"
    [ buffer; pexp_constant ~loc (Pconst_char character) ]

let buffer_add_string ~loc buffer value =
  call ~loc "Buffer.add_string" [ buffer; estr ~loc value ]

let json_helper ~loc name buffer value =
  for_ppx_call ~loc ("json_" ^ name) [ buffer; value ]

let json_descriptor (module Engine : Ppx_repr_lib.Engine.S) ~library description
    buffer value =
  let loc = description.ptyp_loc in
  let description =
    Type_shape.expand_descriptor (module Engine) ~library description
  in
  for_ppx_call ~loc "json" [ description; buffer; value ]

let primitive_json_name = function
  | Type_shape.Unit -> Some "unit"
  | Type_shape.Bool -> Some "bool"
  | Type_shape.Char -> Some "char"
  | Type_shape.Int -> Some "int"
  | Type_shape.Int32 -> Some "int32"
  | Type_shape.Int64 -> Some "int64"
  | Type_shape.Float -> Some "float"
  | Type_shape.String -> Some "string"
  | Type_shape.Bytes -> Some "bytes"
  | Type_shape.List | Type_shape.Array | Type_shape.Option | Type_shape.Lazy ->
      None

let rec json_of_core (module Engine : Ppx_repr_lib.Engine.S) ~library ~encoders
    description buffer value =
  let loc = description.ptyp_loc in
  if Type_shape.has_custom_repr description then
    json_descriptor
      (module Engine : Ppx_repr_lib.Engine.S)
      ~library description buffer value
  else
    match description.ptyp_desc with
    | Ptyp_var name ->
        for_ppx_call ~loc "json" [ evar ~loc name; buffer; value ]
    | Ptyp_constr ({ txt = Lident name; _ }, arguments)
      when List.mem_assoc name encoders ->
        let _ = arguments in
        call ~loc (List.assoc name encoders) [ buffer; value ]
    | Ptyp_constr ({ txt = Lident name; _ }, []) -> (
        match List.assoc_opt name encoders with
        | Some _ -> assert false
        | None -> (
            match
              Option.bind (Type_shape.builtin description) primitive_json_name
            with
            | Some primitive -> json_helper ~loc primitive buffer value
            | None ->
                json_descriptor
                  (module Engine : Ppx_repr_lib.Engine.S)
                  ~library description buffer value))
    | Ptyp_constr (_, [ item ])
      when Type_shape.builtin description = Some Type_shape.List ->
        let item_name = "__observe_json_item" in
        let item_encoder =
          lambda ~loc
            [ pvar ~loc "__observe_json_item_buffer"; pvar ~loc item_name ]
            (json_of_core
               (module Engine : Ppx_repr_lib.Engine.S)
               ~library ~encoders item
               (evar ~loc "__observe_json_item_buffer")
               (evar ~loc item_name))
        in
        for_ppx_call ~loc "json_list" [ item_encoder; buffer; value ]
    | Ptyp_constr (_, [ item ])
      when Type_shape.builtin description = Some Type_shape.Array ->
        let item_name = "__observe_json_item" in
        let item_encoder =
          lambda ~loc
            [ pvar ~loc "__observe_json_item_buffer"; pvar ~loc item_name ]
            (json_of_core
               (module Engine : Ppx_repr_lib.Engine.S)
               ~library ~encoders item
               (evar ~loc "__observe_json_item_buffer")
               (evar ~loc item_name))
        in
        for_ppx_call ~loc "json_array" [ item_encoder; buffer; value ]
    | Ptyp_constr (_, [ item ])
      when Type_shape.builtin description = Some Type_shape.Option ->
        let item_name = "__observe_json_item" in
        let item_encoder =
          lambda ~loc
            [ pvar ~loc "__observe_json_item_buffer"; pvar ~loc item_name ]
            (json_of_core
               (module Engine : Ppx_repr_lib.Engine.S)
               ~library ~encoders item
               (evar ~loc "__observe_json_item_buffer")
               (evar ~loc item_name))
        in
        for_ppx_call ~loc "json_option" [ item_encoder; buffer; value ]
    | Ptyp_constr (_, [ item ])
      when Type_shape.builtin description = Some Type_shape.Lazy ->
        json_of_core
          (module Engine : Ppx_repr_lib.Engine.S)
          ~library ~encoders item buffer
          (call ~loc "Lazy.force" [ value ])
    | Ptyp_tuple descriptions ->
        let names =
          List.mapi
            (fun index _ -> indexed "__observe_json_tuple_" index)
            descriptions
        in
        let writes =
          List.mapi
            (fun index description ->
              let encoded =
                json_of_core
                  (module Engine : Ppx_repr_lib.Engine.S)
                  ~library ~encoders description buffer
                  (evar ~loc (List.nth names index))
              in
              if index = 0 then encoded
              else sequence ~loc [ buffer_add_char ~loc buffer ',' ] encoded)
            descriptions
        in
        pexp_match ~loc value
          [
            case
              ~lhs:(ppat_tuple ~loc (List.map (pvar ~loc) names))
              ~guard:None
              ~rhs:
                (sequence ~loc
                   (buffer_add_char ~loc buffer '[' :: writes)
                   (buffer_add_char ~loc buffer ']'));
          ]
    | Ptyp_variant (rows, Closed, _) ->
        json_of_polyvariant
          (module Engine : Ppx_repr_lib.Engine.S)
          ~library ~encoders ~loc rows buffer value
    | _ ->
        json_descriptor
          (module Engine : Ppx_repr_lib.Engine.S)
          ~library description buffer value

and json_of_polyvariant (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~encoders ~loc rows buffer value =
  let cases =
    List.map
      (fun row ->
        match row.prf_desc with
        | Rtag (label, _, []) ->
            case
              ~lhs:(ppat_variant ~loc:row.prf_loc label.txt None)
              ~guard:None
              ~rhs:
                (json_helper ~loc:row.prf_loc "string" buffer
                   (estr ~loc:row.prf_loc label.txt))
        | Rtag (label, _, descriptions) ->
            let names =
              List.mapi
                (fun index _ -> indexed "__observe_poly_json_" index)
                descriptions
            in
            let pattern =
              tuple_pattern ~loc:row.prf_loc
                (List.map (pvar ~loc:row.prf_loc) names)
            in
            let payload_type =
              match descriptions with
              | [ description ] -> description
              | descriptions -> ptyp_tuple ~loc:row.prf_loc descriptions
            in
            let payload_value =
              tuple_expression ~loc:row.prf_loc
                (List.map (evar ~loc:row.prf_loc) names)
            in
            let payload =
              json_of_core
                (module Engine : Ppx_repr_lib.Engine.S)
                ~library ~encoders payload_type buffer payload_value
            in
            case
              ~lhs:(ppat_variant ~loc:row.prf_loc label.txt (Some pattern))
              ~guard:None
              ~rhs:
                (sequence ~loc:row.prf_loc
                   [
                     buffer_add_string ~loc:row.prf_loc buffer
                       ("{\"" ^ label.txt ^ "\":");
                     payload;
                   ]
                   (buffer_add_char ~loc:row.prf_loc buffer '}'))
        | Rinherit _ ->
            inline_error ~loc:row.prf_loc
              "inherited polymorphic-variant rows are not supported")
      rows
  in
  pexp_match ~loc value cases

let omittable_field description =
  match (Type_shape.builtin description, description.ptyp_desc) with
  | Some Type_shape.Option, Ptyp_constr (_, [ item ]) -> `Option item
  | Some Type_shape.List, Ptyp_constr (_, [ _ ]) -> `List
  | _ -> `Required

let needs_runtime_field_policy description =
  match Type_shape.builtin description with
  | Some _ -> false
  | None -> (
      match description.ptyp_desc with
      | Ptyp_var _ | Ptyp_constr _ -> true
      | Ptyp_any | Ptyp_arrow _ | Ptyp_tuple _ | Ptyp_object _ | Ptyp_class _
      | Ptyp_alias _ | Ptyp_variant _ | Ptyp_poly _ | Ptyp_package _
      | Ptyp_extension _ | Ptyp_open _ ->
          false)

let json_field_write (module Engine : Ppx_repr_lib.Engine.S) ~library ~encoders
    ~loc ~first ~name description buffer value =
  let prefix = "\"" ^ name ^ "\":" in
  let comma_prefix = "," ^ prefix in
  let add_prefix =
    pexp_ifthenelse ~loc first
      (buffer_add_string ~loc buffer prefix)
      (Some (buffer_add_string ~loc buffer comma_prefix))
  in
  let write encoded =
    sequence ~loc [ add_prefix; encoded ] (ebool ~loc false)
  in
  match omittable_field description with
  | `Required ->
      if needs_runtime_field_policy description then
        let expanded =
          Type_shape.expand_descriptor (module Engine) ~library description
        in
        for_ppx_call ~loc "json_field"
          [ expanded; buffer; first; estr ~loc name; value ]
      else
        write
          (json_of_core
             (module Engine : Ppx_repr_lib.Engine.S)
             ~library ~encoders description buffer value)
  | `List ->
      pexp_match ~loc value
        [
          case
            ~lhs:(ppat_construct ~loc (lident ~loc "[]") None)
            ~guard:None ~rhs:first;
          case ~lhs:(ppat_any ~loc) ~guard:None
            ~rhs:
              (write
                 (json_of_core
                    (module Engine : Ppx_repr_lib.Engine.S)
                    ~library ~encoders description buffer value));
        ]
  | `Option item ->
      let some = "__observe_json_some" in
      pexp_match ~loc value
        [
          case
            ~lhs:(ppat_construct ~loc (lident ~loc "None") None)
            ~guard:None ~rhs:first;
          case
            ~lhs:
              (ppat_construct ~loc (lident ~loc "Some") (Some (pvar ~loc some)))
            ~guard:None
            ~rhs:
              (write
                 (json_of_core
                    (module Engine : Ppx_repr_lib.Engine.S)
                    ~library ~encoders item buffer (evar ~loc some)));
        ]

let json_of_named_fields (module Engine : Ppx_repr_lib.Engine.S) ~library
    ~encoders ~loc fields buffer =
  let rec write index first = function
    | [] -> buffer_add_char ~loc buffer '}'
    | (name, description, value) :: rest ->
        let next = indexed "__observe_json_first_" index in
        let encoded =
          json_field_write
            (module Engine : Ppx_repr_lib.Engine.S)
            ~library ~encoders ~loc:description.ptyp_loc ~first ~name
            description buffer value
        in
        pexp_let ~loc Nonrecursive
          [ value_binding ~loc ~pat:(pvar ~loc next) ~expr:encoded ]
          (write (index + 1) (evar ~loc next) rest)
  in
  sequence ~loc
    [ buffer_add_char ~loc buffer '{' ]
    (write 0 (ebool ~loc true) fields)

let json_of_fields (module Engine : Ppx_repr_lib.Engine.S) ~library ~encoders
    fields buffer value =
  let loc = value.pexp_loc in
  let fields =
    List.map
      (fun field ->
        let field_loc = field.pld_loc in
        ( field.pld_name.txt,
          field.pld_type,
          pexp_field ~loc:field_loc value
            (Located.mk ~loc:field_loc (Lident field.pld_name.txt)) ))
      fields
  in
  json_of_named_fields
    (module Engine : Ppx_repr_lib.Engine.S)
    ~library ~encoders ~loc fields buffer

let json_of_variant (module Engine : Ppx_repr_lib.Engine.S) ~library ~encoders
    declaration constructors buffer value =
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
              ~rhs:(json_helper ~loc "string" buffer (estr ~loc name))
        | Pcstr_tuple descriptions ->
            let names =
              List.mapi
                (fun field_index _ ->
                  indexed2 "__observe_json_payload_" index field_index)
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
              json_of_core
                (module Engine : Ppx_repr_lib.Engine.S)
                ~library ~encoders payload_type buffer payload_value
            in
            case
              ~lhs:(constructor_pattern ~loc constructor (Some pattern))
              ~guard:None
              ~rhs:
                (sequence ~loc
                   [
                     buffer_add_string ~loc buffer ("{\"" ^ name ^ "\":");
                     payload;
                   ]
                   (buffer_add_char ~loc buffer '}'))
        | Pcstr_record fields ->
            let binders = field_binders fields in
            let pattern = inline_record_pattern ~loc fields binders in
            let fields =
              List.map2
                (fun field (_, value_name, _) ->
                  (field.pld_name.txt, field.pld_type, evar ~loc value_name))
                fields binders
            in
            let payload =
              json_of_named_fields
                (module Engine : Ppx_repr_lib.Engine.S)
                ~library ~encoders ~loc fields buffer
            in
            case
              ~lhs:(constructor_pattern ~loc constructor (Some pattern))
              ~guard:None
              ~rhs:
                (sequence ~loc
                   [
                     buffer_add_string ~loc buffer ("{\"" ^ name ^ "\":");
                     payload;
                   ]
                   (buffer_add_char ~loc buffer '}')))
      constructors
  in
  pexp_match ~loc:declaration.ptype_loc value cases

let declaration (module Engine : Ppx_repr_lib.Engine.S) ~library ~encoders
    declaration buffer value =
  match declaration.ptype_kind with
  | Ptype_record fields ->
      json_of_fields
        (module Engine : Ppx_repr_lib.Engine.S)
        ~library ~encoders fields buffer value
  | Ptype_variant constructors ->
      json_of_variant
        (module Engine : Ppx_repr_lib.Engine.S)
        ~library ~encoders declaration constructors buffer value
  | Ptype_abstract -> (
      match declaration.ptype_manifest with
      | Some description ->
          json_of_core
            (module Engine : Ppx_repr_lib.Engine.S)
            ~library ~encoders description buffer value
      | None ->
          inline_error ~loc:declaration.ptype_loc
            "abstract types need a manifest")
  | Ptype_open ->
      inline_error ~loc:declaration.ptype_loc "open types are not supported"
