open Ppxlib
open Ast_builder.Default

type builtin =
  | Unit
  | Bool
  | Char
  | Int
  | Int32
  | Int64
  | Float
  | String
  | Bytes
  | List
  | Array
  | Option
  | Lazy

let rec drop_stdlib_prefix = function
  | (Lident _ | Lapply _) as identifier -> identifier
  | Ldot (Lident "Stdlib", suffix) -> Lident suffix
  | Ldot (prefix, suffix) -> Ldot (drop_stdlib_prefix prefix, suffix)

let path_matches identifier name =
  match drop_stdlib_prefix identifier with
  | Lident candidate -> String.equal candidate name
  | Ldot (Lident module_, "t") ->
      String.equal module_ (String.capitalize_ascii name)
  | Ldot _ | Lapply _ -> false

let builtin_of_path identifier =
  if path_matches identifier "unit" then Some Unit
  else if path_matches identifier "bool" then Some Bool
  else if path_matches identifier "char" then Some Char
  else if path_matches identifier "int" then Some Int
  else if path_matches identifier "int32" then Some Int32
  else if path_matches identifier "int64" then Some Int64
  else if path_matches identifier "float" then Some Float
  else if path_matches identifier "string" then Some String
  else if path_matches identifier "bytes" then Some Bytes
  else if path_matches identifier "list" then Some List
  else if path_matches identifier "array" then Some Array
  else if path_matches identifier "option" then Some Option
  else
    match drop_stdlib_prefix identifier with
    | Lident "lazy_t" | Ldot (Lident "Lazy", "t") -> Some Lazy
    | Lident _ | Ldot _ | Lapply _ -> None

let attribute_is_custom attribute =
  match attribute.attr_name.txt with
  | "observe.repr" | "observe.nobuiltin" -> true
  | _ -> false

let has_custom_repr description =
  List.exists attribute_is_custom description.ptyp_attributes

let builtin description =
  if has_custom_repr description then None
  else
    match description.ptyp_desc with
    | Ptyp_constr ({ txt; _ }, _) -> builtin_of_path txt
    | Ptyp_any | Ptyp_var _ | Ptyp_arrow _ | Ptyp_tuple _ | Ptyp_object _
    | Ptyp_class _ | Ptyp_alias _ | Ptyp_variant _ | Ptyp_poly _
    | Ptyp_package _ | Ptyp_extension _ ->
        None

let error ~loc format =
  Location.raise_errorf ~loc ("[@@deriving observe]: " ^^ format)

let validate_core_type description =
  let visitor =
    object
      inherit Ast_traverse.iter as super

      method! core_type description =
        (match description.ptyp_desc with
        | Ptyp_variant (rows, Open, _) ->
            error ~loc:description.ptyp_loc
              "open polymorphic variants are not supported"
        | Ptyp_variant (rows, Closed, _) ->
            List.iter
              (fun row ->
                match row.prf_desc with
                | Rinherit _ ->
                    error ~loc:row.prf_loc
                      "inherited polymorphic-variant rows are not supported"
                | Rtag _ -> ())
              rows
        | Ptyp_arrow _ ->
            error ~loc:description.ptyp_loc "function types are not supported"
        | Ptyp_object _ ->
            error ~loc:description.ptyp_loc "object types are not supported"
        | Ptyp_class _ ->
            error ~loc:description.ptyp_loc "class types are not supported"
        | Ptyp_package _ ->
            error ~loc:description.ptyp_loc "package types are not supported"
        | Ptyp_extension _ ->
            error ~loc:description.ptyp_loc "type extensions are not supported"
        | Ptyp_any | Ptyp_var _ | Ptyp_constr _ | Ptyp_tuple _ | Ptyp_alias _
        | Ptyp_poly _ ->
            ());
        super#core_type description
    end
  in
  visitor#core_type description

let validate_constructor constructor =
  if constructor.pcd_vars <> [] then
    error ~loc:constructor.pcd_loc
      "existential constructor variables are not supported";
  if Option.is_some constructor.pcd_res then
    error ~loc:constructor.pcd_loc "GADT result types are not supported";
  match constructor.pcd_args with
  | Pcstr_tuple descriptions -> List.iter validate_core_type descriptions
  | Pcstr_record fields ->
      List.iter (fun field -> validate_core_type field.pld_type) fields

let validate_declaration declaration =
  List.iter
    (fun (parameter, _) -> validate_core_type parameter)
    declaration.ptype_params;
  match declaration.ptype_kind with
  | Ptype_abstract -> (
      match declaration.ptype_manifest with
      | Some manifest -> validate_core_type manifest
      | None ->
          error ~loc:declaration.ptype_loc "abstract types need a manifest")
  | Ptype_record fields ->
      List.iter (fun field -> validate_core_type field.pld_type) fields
  | Ptype_variant constructors -> List.iter validate_constructor constructors
  | Ptype_open ->
      error ~loc:declaration.ptype_loc "open types are not supported"

let validate_group (rec_flag, declarations) =
  (match (rec_flag, declarations) with
  | Recursive, _ :: _ :: _ :: _ ->
      error ~loc:(List.hd declarations).ptype_loc
        "recursive groups larger than two declarations are not supported"
  | Recursive, [ left; right ]
    when left.ptype_params <> [] || right.ptype_params <> [] ->
      error ~loc:left.ptype_loc
        "parameterized mutually-recursive groups are not supported"
  | Nonrecursive, _ | Recursive, [] | Recursive, [ _ ] | Recursive, [ _; _ ] ->
      ());
  List.iter validate_declaration declarations

let free_parameters description =
  let visitor =
    object
      inherit [string list * string list] Ast_traverse.fold as super

      method! core_type_desc node (seen, parameters) =
        match node with
        | Ptyp_var name when not (List.mem name seen) ->
            (name :: seen, name :: parameters)
        | Ptyp_var _ -> (seen, parameters)
        | _ -> super#core_type_desc node (seen, parameters)
    end
  in
  let _, parameters = visitor#core_type description ([], []) in
  List.rev parameters

let expand_descriptor (module Engine : Ppx_repr_lib.Engine.S) ~library
    description =
  let loc = description.ptyp_loc in
  let expanded = Engine.expand_typ ?lib:library description in
  match free_parameters description with
  | [] -> expanded
  | parameters ->
      pexp_apply ~loc expanded
        (List.map
           (fun name ->
             (Nolabel, pexp_ident ~loc (Located.mk ~loc (Longident.Lident name))))
           parameters)
