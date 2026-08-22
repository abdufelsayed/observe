type 'a field_policy = Required | Omit_if of ('a -> bool)

type 'a t = {
  repr : 'a Repr.t;
  json : Buffer.t -> 'a -> unit;
  plan : 'a -> Pretty.rendered;
  freeze_supported : bool;
  freeze :
    Snapshot.context ->
    depth:int ->
    'a ->
    (Snapshot.value, Snapshot.error) result;
  record_name : string option;
  field_policy : 'a field_policy;
}
(** A description keeps the Repr machine for interoperability together with
    Observe's direct writers. [plan] classifies and projects a value exactly
    once per pretty render and returns the rendering step that owns the value's
    placement. *)

let repr description = description.repr
let plan description value = description.plan value

let pretty description renderer placement value =
  Pretty.render renderer placement (description.plan value)

let add_field_name buffer ~first name =
  if not first then Buffer.add_char buffer ',';
  Json_writer.name buffer name

let unsupported_freeze _ ~depth:_ _ = Result.Error Snapshot.Unsupported

let guard_freeze freeze context ~depth value =
  match Snapshot.check_depth ~depth with
  | Error _ as error -> error
  | Ok () -> freeze context ~depth value

let make ?omit_field ?freeze ?record_name ~plan repr json =
  let field_policy =
    match omit_field with None -> Required | Some omit -> Omit_if omit
  in
  let freeze_supported, freeze =
    match freeze with
    | Some freeze -> (true, freeze)
    | None -> (false, unsupported_freeze)
  in
  {
    repr;
    json;
    plan;
    freeze_supported;
    freeze = guard_freeze freeze;
    record_name;
    field_policy;
  }

let compatibility ?omit_field repr json =
  make ?omit_field
    ~plan:(fun value ->
      Repr_projection.plan_node (Repr_projection.of_repr repr value))
    repr json

let write_field description buffer first name value =
  match description.field_policy with
  | Omit_if omit when omit value -> first
  | Required | Omit_if _ ->
      add_field_name buffer ~first name;
      description.json buffer value;
      false

let with_json description json = { description with json }
let with_plan description plan = { description with plan }

let with_freeze description freeze =
  { description with freeze_supported = true; freeze = guard_freeze freeze }

let with_repr description repr = { description with repr }

let freeze_into description context ~depth value =
  description.freeze context ~depth value

let freeze description value =
  if not description.freeze_supported then Error Snapshot.Unsupported
  else
    let context = Snapshot.create_context () in
    match description.freeze context ~depth:0 value with
    | Ok value -> Ok (Snapshot.seal context value)
    | Error _ as error -> error

let record_name description = description.record_name

let repr_json repr buffer value =
  Buffer.add_string buffer (Repr.to_json_string ~minify:true repr value)

let of_repr ?json repr =
  let json = Option.value json ~default:(repr_json repr) in
  compatibility repr json

type 'a description = 'a t

open Pretty

let scalar_description ?omit_field ~freeze repr json append =
  make ?omit_field ~freeze
    ~plan:(fun value -> Scalar (fun renderer -> append renderer value))
    repr json

let unit =
  scalar_description
    ~freeze:(fun context ~depth () -> Snapshot.object_ context ~depth [])
    Repr.unit Json_writer.unit
    (fun renderer () -> empty_record renderer)

let bool =
  scalar_description
    ~freeze:(fun context ~depth value -> Snapshot.bool context ~depth value)
    Repr.bool Json_writer.bool bool

let char =
  scalar_description
    ~freeze:(fun context ~depth value ->
      Snapshot.string context ~depth (String.make 1 value))
    Repr.char Json_writer.char
    (fun renderer value -> string renderer (String.make 1 value))

let int =
  scalar_description
    ~freeze:(fun context ~depth value -> Snapshot.int context ~depth value)
    Repr.int Json_writer.int int

let int32 =
  scalar_description
    ~freeze:(fun context ~depth value -> Snapshot.int32 context ~depth value)
    Repr.int32 Json_writer.int32 int32

let int63 =
  scalar_description
    ~freeze:(fun context ~depth value ->
      Snapshot.int64 context ~depth (Optint.Int63.to_int64 value))
    Repr.int63
    (fun buffer value -> Json_writer.int64 buffer (Optint.Int63.to_int64 value))
    (fun renderer value -> number renderer (Repr.to_string Repr.int63 value))

let int64 =
  scalar_description
    ~freeze:(fun context ~depth value -> Snapshot.int64 context ~depth value)
    Repr.int64 Json_writer.int64 int64

let float =
  scalar_description
    ~freeze:(fun context ~depth value -> Snapshot.float context ~depth value)
    Repr.float Json_writer.float float

let string =
  scalar_description
    ~freeze:(fun context ~depth value -> Snapshot.string context ~depth value)
    Repr.string Json_writer.string string

let bytes =
  scalar_description
    ~freeze:(fun context ~depth value -> Snapshot.bytes context ~depth value)
    Repr.bytes Json_writer.bytes
    (fun renderer value ->
      Pretty.string renderer (Bytes.unsafe_to_string value))

type len = Repr.len

let string_of length = with_repr string (Repr.string_of length)
let bytes_of length = with_repr bytes (Repr.bytes_of length)
let boxed description = with_repr description (Repr.boxed description.repr)
let is_scalar = function Scalar _ -> true | Node _ -> false

let append_scalar_plans renderer plans =
  list_start renderer;
  let rec append_plans = function
    | [] -> ()
    | [ Scalar write ] -> write renderer
    | Scalar write :: rest ->
        write renderer;
        list_separator renderer;
        append_plans rest
    | Node _ :: _ -> invalid_arg "Observe.Type: non-scalar plan in scalar list"
  in
  append_plans plans;
  list_end renderer

let append_tree_plans renderer plans =
  let rec append index = function
    | [] -> ()
    | [ planned ] -> render renderer (Index { last = true; index }) planned
    | planned :: rest ->
        render renderer (Index { last = false; index }) planned;
        newline renderer;
        append (index + 1) rest
  in
  append 0 plans

let plan_list description values =
  match List.map description.plan values with
  | [] -> Scalar (fun renderer -> empty_list renderer)
  | plans when List.for_all is_scalar plans ->
      Scalar (fun renderer -> append_scalar_plans renderer plans)
  | plans ->
      Node
        (fun renderer placement ->
          let nested = place renderer placement ~scalar:false in
          append_tree_plans renderer plans;
          finish renderer nested)

let freeze_list_values description context ~depth values =
  let rec collect count frozen = function
    | [] -> Snapshot.list context ~depth (List.rev frozen)
    | _ when count >= Snapshot.width_limit ->
        Result.Error Snapshot.Limit_exceeded
    | value :: rest -> (
        match description.freeze context ~depth:(depth + 1) value with
        | Result.Error _ as error -> error
        | Ok value -> collect (count + 1) (value :: frozen) rest)
  in
  collect 0 [] values

let freeze_sequence_values description context ~depth values =
  let rec collect count frozen values =
    match values () with
    | Seq.Nil -> Snapshot.list context ~depth (List.rev frozen)
    | Seq.Cons _ when count >= Snapshot.width_limit ->
        Result.Error Snapshot.Limit_exceeded
    | Seq.Cons (value, rest) -> (
        match description.freeze context ~depth:(depth + 1) value with
        | Result.Error _ as error -> error
        | Ok value -> collect (count + 1) (value :: frozen) rest)
  in
  collect 0 [] values

let list ?len description =
  make
    ~omit_field:(function [] -> true | _ :: _ -> false)
    ~plan:(plan_list description)
    ~freeze:(fun context ~depth values ->
      freeze_list_values description context ~depth values)
    (Repr.list ?len description.repr)
    (Json_writer.list description.json)

let array ?len description =
  let plan values =
    match List.map description.plan (Array.to_list values) with
    | [] -> Scalar (fun renderer -> empty_list renderer)
    | plans when List.for_all is_scalar plans ->
        Scalar (fun renderer -> append_scalar_plans renderer plans)
    | plans ->
        Node
          (fun renderer placement ->
            let nested = place renderer placement ~scalar:false in
            append_tree_plans renderer plans;
            finish renderer nested)
  in
  let freeze context ~depth values =
    let length = Array.length values in
    if length > Snapshot.width_limit then Result.Error Snapshot.Limit_exceeded
    else
      let rec collect index frozen =
        if index = length then Snapshot.list context ~depth (List.rev frozen)
        else
          match
            description.freeze context ~depth:(depth + 1) values.(index)
          with
          | Result.Error _ as error -> error
          | Ok value -> collect (index + 1) (value :: frozen)
      in
      collect 0 []
  in
  make ~plan ~freeze
    (Repr.array ?len description.repr)
    (Json_writer.array description.json)

let option description =
  make
    ~omit_field:(function None -> true | Some _ -> false)
    ~plan:(function
      | None -> Scalar (fun renderer -> null renderer)
      | Some value -> description.plan value)
    ~freeze:(fun context ~depth -> function
      | None -> Snapshot.null context ~depth
      | Some value -> description.freeze context ~depth value)
    (Repr.option description.repr)
    (Json_writer.option description.json)

let pair left right =
  let json buffer (left_value, right_value) =
    Buffer.add_char buffer '[';
    left.json buffer left_value;
    Buffer.add_char buffer ',';
    right.json buffer right_value;
    Buffer.add_char buffer ']'
  in
  let plan (left_value, right_value) =
    let plans = [ left.plan left_value; right.plan right_value ] in
    if List.for_all is_scalar plans then
      Scalar (fun renderer -> append_scalar_plans renderer plans)
    else
      Node
        (fun renderer placement ->
          let nested = place renderer placement ~scalar:false in
          append_tree_plans renderer plans;
          finish renderer nested)
  in
  let freeze context ~depth (left_value, right_value) =
    match left.freeze context ~depth:(depth + 1) left_value with
    | Result.Error _ as error -> error
    | Ok left_value -> (
        match right.freeze context ~depth:(depth + 1) right_value with
        | Result.Error _ as error -> error
        | Ok right_value ->
            Snapshot.list context ~depth [ left_value; right_value ])
  in
  make ~plan ~freeze (Repr.pair left.repr right.repr) json

let triple first second third =
  let json buffer (first_value, second_value, third_value) =
    Buffer.add_char buffer '[';
    first.json buffer first_value;
    Buffer.add_char buffer ',';
    second.json buffer second_value;
    Buffer.add_char buffer ',';
    third.json buffer third_value;
    Buffer.add_char buffer ']'
  in
  let plan (a, b, c) =
    let plans = [ first.plan a; second.plan b; third.plan c ] in
    if List.for_all is_scalar plans then
      Scalar (fun renderer -> append_scalar_plans renderer plans)
    else
      Node
        (fun renderer placement ->
          let nested = place renderer placement ~scalar:false in
          append_tree_plans renderer plans;
          finish renderer nested)
  in
  let freeze context ~depth (a, b, c) =
    match first.freeze context ~depth:(depth + 1) a with
    | Result.Error _ as error -> error
    | Ok a -> (
        match second.freeze context ~depth:(depth + 1) b with
        | Result.Error _ as error -> error
        | Ok b -> (
            match third.freeze context ~depth:(depth + 1) c with
            | Result.Error _ as error -> error
            | Ok c -> Snapshot.list context ~depth [ a; b; c ]))
  in
  make ~plan ~freeze (Repr.triple first.repr second.repr third.repr) json

let quad first second third fourth =
  let json buffer (first_value, second_value, third_value, fourth_value) =
    Buffer.add_char buffer '[';
    first.json buffer first_value;
    Buffer.add_char buffer ',';
    second.json buffer second_value;
    Buffer.add_char buffer ',';
    third.json buffer third_value;
    Buffer.add_char buffer ',';
    fourth.json buffer fourth_value;
    Buffer.add_char buffer ']'
  in
  let plan (a, b, c, d) =
    let plans = [ first.plan a; second.plan b; third.plan c; fourth.plan d ] in
    if List.for_all is_scalar plans then
      Scalar (fun renderer -> append_scalar_plans renderer plans)
    else
      Node
        (fun renderer placement ->
          let nested = place renderer placement ~scalar:false in
          append_tree_plans renderer plans;
          finish renderer nested)
  in
  let freeze context ~depth (a, b, c, d) =
    match first.freeze context ~depth:(depth + 1) a with
    | Result.Error _ as error -> error
    | Ok a -> (
        match second.freeze context ~depth:(depth + 1) b with
        | Result.Error _ as error -> error
        | Ok b -> (
            match third.freeze context ~depth:(depth + 1) c with
            | Result.Error _ as error -> error
            | Ok c -> (
                match fourth.freeze context ~depth:(depth + 1) d with
                | Result.Error _ as error -> error
                | Ok d -> Snapshot.list context ~depth [ a; b; c; d ])))
  in
  make ~plan ~freeze
    (Repr.quad first.repr second.repr third.repr fourth.repr)
    json

let result ok error =
  let json buffer = function
    | Ok value ->
        Buffer.add_string buffer "{\"ok\":";
        ok.json buffer value;
        Buffer.add_char buffer '}'
    | Error value ->
        Buffer.add_string buffer "{\"error\":";
        error.json buffer value;
        Buffer.add_char buffer '}'
  in
  let plan = function
    | Ok value ->
        Node
          (fun renderer placement ->
            let nested = place renderer placement ~scalar:false in
            render renderer
              (Constructor { last = true; name = "Ok" })
              (ok.plan value);
            finish renderer nested)
    | Error value ->
        Node
          (fun renderer placement ->
            let nested = place renderer placement ~scalar:false in
            render renderer
              (Constructor { last = true; name = "Error" })
              (error.plan value);
            finish renderer nested)
  in
  let freeze context ~depth = function
    | Ok value -> (
        match ok.freeze context ~depth:(depth + 1) value with
        | Result.Error _ as error -> error
        | Ok value -> Snapshot.object_ context ~depth [ ("ok", value) ])
    | Error value -> (
        match error.freeze context ~depth:(depth + 1) value with
        | Result.Error _ as error -> error
        | Ok value -> Snapshot.object_ context ~depth [ ("error", value) ])
  in
  make ~plan ~freeze (Repr.result ok.repr error.repr) json

let seq description =
  let repr = Repr.seq description.repr in
  let json buffer values =
    Buffer.add_char buffer '[';
    let rec write first values =
      match values () with
      | Seq.Nil -> ()
      | Seq.Cons (value, rest) ->
          if not first then Buffer.add_char buffer ',';
          description.json buffer value;
          write false rest
    in
    write true values;
    Buffer.add_char buffer ']'
  in
  (* The sequence is enumerated exactly once per render. The plan inspects the
     first cell to choose a layout and the rendering step continues from that
     cell, so effectful sequences are neither pre-consumed nor restarted. *)
  let plan values =
    match values () with
    | Seq.Nil -> Scalar (fun renderer -> empty_list renderer)
    | Seq.Cons _ as node ->
        Node
          (fun renderer placement ->
            let nested = place renderer placement ~scalar:false in
            let rec append index node =
              match node with
              | Seq.Nil -> ()
              | Seq.Cons (value, rest) -> (
                  let next = rest () in
                  let last =
                    match next with Seq.Nil -> true | Seq.Cons _ -> false
                  in
                  render renderer
                    (Index { last; index })
                    (description.plan value);
                  if not last then newline renderer;
                  match next with
                  | Seq.Nil -> ()
                  | Seq.Cons _ -> append (index + 1) next)
            in
            append 0 node;
            finish renderer nested)
  in
  let freeze context ~depth values =
    freeze_sequence_values description context ~depth values
  in
  make ~plan ~freeze repr json

let ref description =
  make
    ~plan:(fun value -> description.plan !value)
    ~freeze:(fun context ~depth value ->
      description.freeze context ~depth !value)
    (Repr.ref description.repr)
    (fun buffer value -> description.json buffer !value)

let lazy_t description =
  make
    ~plan:(fun value -> description.plan (Lazy.force value))
    ~freeze:(fun context ~depth value ->
      description.freeze context ~depth (Lazy.force value))
    (Repr.lazy_t description.repr)
    (fun buffer value -> description.json buffer (Lazy.force value))

let plan_collected plans =
  if List.for_all is_scalar plans then
    Scalar (fun renderer -> append_scalar_plans renderer plans)
  else
    Node
      (fun renderer placement ->
        let nested = place renderer placement ~scalar:false in
        append_tree_plans renderer plans;
        finish renderer nested)

let queue description =
  let repr = Repr.queue description.repr in
  let sequence = seq description in
  let json buffer values = sequence.json buffer (Queue.to_seq values) in
  let plan values =
    let plans =
      Queue.fold (fun plans value -> description.plan value :: plans) [] values
      |> List.rev
    in
    match plans with
    | [] -> Scalar (fun renderer -> empty_list renderer)
    | _ -> plan_collected plans
  in
  let freeze context ~depth values =
    freeze_sequence_values description context ~depth (Queue.to_seq values)
  in
  make ~plan ~freeze repr json

let stack description =
  let repr = Repr.stack description.repr in
  let sequence = seq description in
  let json buffer values = sequence.json buffer (Stack.to_seq values) in
  let plan values =
    let plans =
      Stack.fold (fun plans value -> description.plan value :: plans) [] values
      |> List.rev
    in
    match plans with
    | [] -> Scalar (fun renderer -> empty_list renderer)
    | _ -> plan_collected plans
  in
  let freeze context ~depth values =
    freeze_sequence_values description context ~depth (Stack.to_seq values)
  in
  make ~plan ~freeze repr json

let hashtbl key value =
  let repr = Repr.hashtbl key.repr value.repr in
  let entry = pair key value in
  let entries = seq entry in
  let json buffer table = entries.json buffer (Hashtbl.to_seq table) in
  let plan table =
    let plans =
      Hashtbl.fold
        (fun key value plans -> entry.plan (key, value) :: plans)
        table []
      |> List.rev
    in
    match plans with
    | [] -> Scalar (fun renderer -> empty_list renderer)
    | _ -> plan_collected plans
  in
  let freeze context ~depth table =
    freeze_sequence_values entry context ~depth (Hashtbl.to_seq table)
  in
  make ~plan ~freeze repr json

type empty = Repr.empty = |

let empty = of_repr Repr.empty

type ('a, 'b, 'c) open_record = {
  record_name : string;
  repr_record : ('a, 'b, 'c) Repr.open_record;
  json_fields_rev : (Buffer.t -> first:bool -> 'a -> bool) list;
  plan_fields_rev : ('a -> string * Pretty.rendered) list;
  freeze_fields_rev :
    (Snapshot.context ->
    depth:int ->
    'a ->
    ((string * Snapshot.value) option, Snapshot.error) result)
    list;
}

type ('a, 'b) field = {
  repr_field : ('a, 'b) Repr.field;
  json_record_field : Buffer.t -> first:bool -> 'a -> bool;
  plan_record_field : 'a -> string * Pretty.rendered;
  freeze_record_field :
    Snapshot.context ->
    depth:int ->
    'a ->
    ((string * Snapshot.value) option, Snapshot.error) result;
}

let record name constructor =
  {
    record_name = name;
    repr_record = Repr.record name constructor;
    json_fields_rev = [];
    plan_fields_rev = [];
    freeze_fields_rev = [];
  }

let field name description getter =
  let owned_name = Snapshot.own_text name in
  {
    repr_field = Repr.field name description.repr getter;
    json_record_field =
      (fun buffer ~first record ->
        write_field description buffer first name (getter record));
    plan_record_field = (fun record -> (name, description.plan (getter record)));
    freeze_record_field =
      (fun context ~depth record ->
        match owned_name with
        | Error _ as error -> error
        | Ok name -> (
            let value = getter record in
            match description.field_policy with
            | Omit_if omit when omit value -> Ok None
            | Required | Omit_if _ ->
                Result.map
                  (fun value -> Some (name, value))
                  (description.freeze context ~depth value)));
  }

let ( |+ ) record field =
  {
    repr_record = Repr.( |+ ) record.repr_record field.repr_field;
    record_name = record.record_name;
    json_fields_rev = field.json_record_field :: record.json_fields_rev;
    plan_fields_rev = field.plan_record_field :: record.plan_fields_rev;
    freeze_fields_rev = field.freeze_record_field :: record.freeze_fields_rev;
  }

let sealr record =
  let repr = Repr.sealr record.repr_record in
  let json_fields = List.rev record.json_fields_rev in
  let plan_fields = List.rev record.plan_fields_rev in
  let freeze_fields = List.rev record.freeze_fields_rev in
  let json buffer value =
    Buffer.add_char buffer '{';
    let rec write first = function
      | [] -> ()
      | field :: rest -> write (field buffer ~first value) rest
    in
    write true json_fields;
    Buffer.add_char buffer '}'
  in
  let plan value =
    let planned = List.map (fun field -> field value) plan_fields in
    match planned with
    | [] -> Scalar (fun renderer -> empty_record renderer)
    | fields ->
        Node
          (fun renderer placement ->
            let nested = place renderer placement ~scalar:false in
            let rec append = function
              | [] -> ()
              | [ (name, field_plan) ] ->
                  render renderer (Field { last = true; name }) field_plan
              | (name, field_plan) :: rest ->
                  render renderer (Field { last = false; name }) field_plan;
                  newline renderer;
                  append rest
            in
            append fields;
            finish renderer nested)
  in
  let freeze context ~depth value =
    let rec collect fields frozen =
      match fields with
      | [] -> Snapshot.object_with_owned_names context ~depth (List.rev frozen)
      | field :: rest -> (
          match field context ~depth:(depth + 1) value with
          | Result.Error _ as error -> error
          | Ok None -> collect rest frozen
          | Ok (Some field) -> collect rest (field :: frozen))
    in
    collect freeze_fields []
  in
  make ~plan ~freeze ~record_name:record.record_name repr json

type 'a json_case =
  | Json0 of { name : string; polymorphic : bool; matches : 'a -> bool }
  | Json1 : {
      name : string;
      polymorphic : bool;
      description : 'b description;
      project : 'a -> 'b option;
    }
      -> 'a json_case

type 'a selected_case =
  | Selected0 of string * bool
  | Selected1 : string * bool * 'b description * 'b -> 'a selected_case

type ('a, 'b, 'c) open_variant = {
  repr_variant : ('a, 'b, 'c) Repr.open_variant;
  json_cases_rev : 'a json_case option list;
}

type ('a, 'b) case = {
  repr_case : ('a, 'b) Repr.case;
  json_case : 'a json_case option;
}

type 'a case_p = 'a Repr.case_p

let variant name deconstruct =
  { repr_variant = Repr.variant name deconstruct; json_cases_rev = [] }

let case0 ?(polymorphic = false) ?is name value =
  {
    repr_case = Repr.case0 name value;
    json_case =
      Option.map (fun matches -> Json0 { name; polymorphic; matches }) is;
  }

let case1 ?(polymorphic = false) ?project name description inject =
  {
    repr_case = Repr.case1 name description.repr inject;
    json_case =
      Option.map
        (fun project -> Json1 { name; polymorphic; description; project })
        project;
  }

let ( |~ ) variant case =
  {
    repr_variant = Repr.( |~ ) variant.repr_variant case.repr_case;
    json_cases_rev = case.json_case :: variant.json_cases_rev;
  }

let sealv variant =
  let repr = Repr.sealv variant.repr_variant in
  let rec collect selectors missing = function
    | [] -> (selectors, missing)
    | None :: rest -> collect selectors (missing + 1) rest
    | Some _ :: rest -> collect (selectors + 1) missing rest
  in
  let selectors, missing = collect 0 0 variant.json_cases_rev in
  if selectors > 0 && missing > 0 then
    invalid_arg
      "Observe.Type.sealv: cases mix direct selectors and compatibility-only \
       cases; pass ~is/~project for every case or for none";
  let rec collect_cases collected = function
    | [] -> Some collected
    | None :: _ -> None
    | Some case :: rest -> collect_cases (case :: collected) rest
  in
  match collect_cases [] variant.json_cases_rev with
  | None -> compatibility repr (repr_json repr)
  | Some cases ->
      let rec find value = function
        | [] -> None
        | Json0 { name; polymorphic; matches } :: rest ->
            if matches value then Some (Selected0 (name, polymorphic))
            else find value rest
        | Json1 { name; polymorphic; description; project } :: rest -> (
            match project value with
            | None -> find value rest
            | Some payload ->
                Some (Selected1 (name, polymorphic, description, payload)))
      in
      let json buffer value =
        match find value cases with
        | None ->
            invalid_arg "Observe.Type.sealv: deconstructor matched no case"
        | Some (Selected0 (name, _)) -> Json_writer.string buffer name
        | Some (Selected1 (name, _, description, payload)) ->
            Buffer.add_char buffer '{';
            Json_writer.name buffer name;
            description.json buffer payload;
            Buffer.add_char buffer '}'
      in
      let plan value =
        match find value cases with
        | None -> raise (Pretty.Error Unsupported_value)
        | Some (Selected0 (name, polymorphic)) ->
            Scalar (fun renderer -> Pretty.variant renderer ~polymorphic name)
        | Some (Selected1 (name, polymorphic, description, payload)) ->
            Node
              (fun renderer placement ->
                let name = if polymorphic then "`" ^ name else name in
                let nested = place renderer placement ~scalar:false in
                render renderer
                  (Constructor { last = true; name })
                  (description.plan payload);
                finish renderer nested)
      in
      let freeze context ~depth value =
        match find value cases with
        | None -> Result.Error Snapshot.Conversion_failed
        | Some (Selected0 (name, polymorphic)) ->
            Snapshot.variant context ~depth ~polymorphic name None
        | Some (Selected1 (name, polymorphic, description, payload)) -> (
            match description.freeze context ~depth:(depth + 1) payload with
            | Result.Error _ as error -> error
            | Ok payload ->
                Snapshot.variant context ~depth ~polymorphic name (Some payload)
            )
      in
      make ~plan ~freeze repr json

let enum name cases =
  let repr = Repr.enum name cases in
  let equal = Repr.unstage (Repr.equal repr) in
  let find value =
    List.find_opt (fun (_, candidate) -> equal value candidate) cases
  in
  let json buffer value =
    match find value with
    | Some (name, _) -> Json_writer.string buffer name
    | None -> repr_json repr buffer value
  in
  let plan value =
    match find value with
    | Some (name, _) ->
        Scalar (fun renderer -> Pretty.variant renderer ~polymorphic:false name)
    | None -> raise (Pretty.Error Unsupported_value)
  in
  let freeze context ~depth value =
    match find value with
    | Some (name, _) ->
        Snapshot.variant context ~depth ~polymorphic:false name None
    | None -> Result.Error Snapshot.Conversion_failed
  in
  make ~plan ~freeze repr json

(* [mu] forwards through one coherent writer record. [Repr.mu] may re-invoke
   the builder when a generic that unrolls (pretty printing, equality, size,
   binary staging) is staged, so the cell is assigned once: later unrolls keep
   the first writers. Builders must be pure and must not force the recursive
   description during construction. *)
let mu (type value) (make_description : value description -> value description)
    =
  let cell = Stdlib.ref None in
  let used_too_early op =
    invalid_arg
      ("Observe.Type.mu: recursive " ^ op ^ " used during construction")
  in
  let forward_json buffer value =
    match !cell with
    | Some description -> description.json buffer value
    | None -> used_too_early "JSON writer"
  in
  let forward_plan value =
    match !cell with
    | Some description -> description.plan value
    | None -> used_too_early "pretty plan"
  in
  let forward_freeze context ~depth value =
    match !cell with
    | Some description -> description.freeze context ~depth value
    | None -> used_too_early "freezer"
  in
  let repr =
    Repr.mu (fun machine ->
        let recursive =
          {
            repr = machine;
            json = forward_json;
            plan = forward_plan;
            freeze_supported = true;
            freeze = forward_freeze;
            record_name = None;
            field_policy = Required;
          }
        in
        let description = make_description recursive in
        (match !cell with None -> cell := Some description | Some _ -> ());
        description.repr)
  in
  {
    repr;
    json = forward_json;
    plan = forward_plan;
    freeze_supported = true;
    freeze = forward_freeze;
    record_name =
      (match !cell with
      | Some description -> description.record_name
      | None -> None);
    field_policy = Required;
  }

let mu2 (type left right)
    (make_descriptions :
      left description ->
      right description ->
      left description * right description) =
  let cell = Stdlib.ref None in
  let used_too_early op =
    invalid_arg
      ("Observe.Type.mu2: recursive " ^ op ^ " used during construction")
  in
  let forward_json_left buffer value =
    match !cell with
    | Some (description, _) -> description.json buffer value
    | None -> used_too_early "left JSON writer"
  in
  let forward_json_right buffer value =
    match !cell with
    | Some (_, description) -> description.json buffer value
    | None -> used_too_early "right JSON writer"
  in
  let forward_plan_left value =
    match !cell with
    | Some (description, _) -> description.plan value
    | None -> used_too_early "left pretty plan"
  in
  let forward_plan_right value =
    match !cell with
    | Some (_, description) -> description.plan value
    | None -> used_too_early "right pretty plan"
  in
  let forward_freeze_left context ~depth value =
    match !cell with
    | Some (description, _) -> description.freeze context ~depth value
    | None -> used_too_early "left freezer"
  in
  let forward_freeze_right context ~depth value =
    match !cell with
    | Some (_, description) -> description.freeze context ~depth value
    | None -> used_too_early "right freezer"
  in
  let left_repr, right_repr =
    Repr.mu2 (fun left_machine right_machine ->
        let left_recursive =
          {
            repr = left_machine;
            json = forward_json_left;
            plan = forward_plan_left;
            freeze_supported = true;
            freeze = forward_freeze_left;
            record_name = None;
            field_policy = Required;
          }
        in
        let right_recursive =
          {
            repr = right_machine;
            json = forward_json_right;
            plan = forward_plan_right;
            freeze_supported = true;
            freeze = forward_freeze_right;
            record_name = None;
            field_policy = Required;
          }
        in
        let left_description, right_description =
          make_descriptions left_recursive right_recursive
        in
        (match !cell with
        | None -> cell := Some (left_description, right_description)
        | Some _ -> ());
        (left_description.repr, right_description.repr))
  in
  ( {
      repr = left_repr;
      json = forward_json_left;
      plan = forward_plan_left;
      freeze_supported = true;
      freeze = forward_freeze_left;
      record_name =
        (match !cell with
        | Some (description, _) -> description.record_name
        | None -> None);
      field_policy = Required;
    },
    {
      repr = right_repr;
      json = forward_json_right;
      plan = forward_plan_right;
      freeze_supported = true;
      freeze = forward_freeze_right;
      record_name =
        (match !cell with
        | Some (_, description) -> description.record_name
        | None -> None);
      field_policy = Required;
    } )

type +'a staged = 'a Repr.staged

let stage = Repr.stage
let unstage = Repr.unstage

type 'a equal = 'a Repr.equal
type 'a compare = 'a Repr.compare
type 'a pp = 'a Repr.pp
type 'a of_string = 'a Repr.of_string
type 'a encode_json = 'a Repr.encode_json
type 'a decode_json = 'a Repr.decode_json
type 'a encode_bin = 'a Repr.encode_bin
type 'a decode_bin = 'a Repr.decode_bin
type -'a size_of = 'a Repr.size_of
type 'a impl = 'a Repr.impl = Structural | Custom of 'a | Undefined

let equal description = Repr.equal description.repr
let compare description = Repr.compare description.repr
let pp description = Repr.pp description.repr
let pp_dump description = Repr.pp_dump description.repr
let to_string description = Repr.to_string description.repr
let of_string description = Repr.of_string description.repr
let encode_json description = Repr.encode_json description.repr
let decode_json description = Repr.decode_json description.repr
let decode_json_lexemes description = Repr.decode_json_lexemes description.repr

let append_json buffer description value =
  (* Transactional: a failed append leaves the buffer at its length at entry,
     so callers never observe partial output of one value. *)
  let start = Buffer.length buffer in
  try description.json buffer value with
  | Json_writer.Invalid_utf8 -> (
      Buffer.truncate buffer start;
      try repr_json description.repr buffer value
      with raised ->
        Buffer.truncate buffer start;
        raise raised)
  | raised ->
      Buffer.truncate buffer start;
      raise raised

let to_json_string description value =
  let buffer = Buffer.create 256 in
  append_json buffer description value;
  Buffer.contents buffer

let of_json_string description = Repr.of_json_string description.repr
let encode_bin description = Repr.encode_bin description.repr
let decode_bin description = Repr.decode_bin description.repr
let to_bin_string description = Repr.to_bin_string description.repr
let of_bin_string description = Repr.of_bin_string description.repr
let size_of description = Repr.size_of description.repr

let like ?pp ?of_string ?json ?bin ?unboxed_bin ?equal ?compare ?short_hash
    ?pre_hash description =
  let repr =
    Repr.like ?pp ?of_string ?json ?bin ?unboxed_bin ?equal ?compare ?short_hash
      ?pre_hash description.repr
  in
  match json with
  | None -> with_repr description repr
  | Some _ ->
      (* A custom Repr JSON machine is an opaque boundary: the direct writers
         can no longer be assumed to agree with it. *)
      compatibility repr (repr_json repr)

let partially_abstract ~pp ~of_string ~json ~bin ~unboxed_bin ~equal ~compare
    ~short_hash ~pre_hash description =
  let repr =
    Repr.partially_abstract ~pp ~of_string ~json ~bin ~unboxed_bin ~equal
      ~compare ~short_hash ~pre_hash description.repr
  in
  match json with
  | Structural -> with_repr description repr
  | Custom _ | Undefined -> compatibility repr (repr_json repr)

let map description decode encode =
  let repr = Repr.map description.repr decode encode in
  let json buffer value = description.json buffer (encode value) in
  let plan value = description.plan (encode value) in
  let freeze context ~depth value =
    description.freeze context ~depth (encode value)
  in
  make ~plan ~freeze repr json

module Generated_runtime = struct
  type renderer = Pretty.t
  type placement = Pretty.placement

  type rendered = Pretty.rendered =
    | Scalar of (renderer -> unit)
    | Node of (renderer -> placement -> unit)

  let with_json = with_json
  let with_plan = with_plan
  let with_freeze = with_freeze

  let with_recursive_plan description make_plan =
    let self = Stdlib.ref None in
    let plan value =
      let description =
        match !self with
        | Some description -> description
        | None ->
            invalid_arg "Observe.Generated_runtime: recursive plan unavailable"
      in
      make_plan description value
    in
    let description = with_plan description plan in
    self := Some description;
    description

  let with_recursive_freeze description make_freeze =
    let self = Stdlib.ref None in
    let freeze context ~depth value =
      let description =
        match !self with
        | Some description -> description
        | None ->
            invalid_arg
              "Observe.Generated_runtime: recursive freezer unavailable"
      in
      make_freeze description context ~depth value
    in
    let description = with_freeze description freeze in
    self := Some description;
    description

  type freeze_context = Snapshot.context
  type frozen = Snapshot.value
  type freeze_error = Snapshot.error

  let freeze description context ~depth value =
    description.freeze context ~depth value

  let frozen_string = Snapshot.string
  let frozen_object = Snapshot.object_

  let frozen_variant context ~depth polymorphic name =
    Snapshot.variant context ~depth ~polymorphic name None

  let frozen_variant_payload context ~depth polymorphic name payload =
    Snapshot.variant context ~depth ~polymorphic name (Some payload)

  let json description buffer value = description.json buffer value
  let plan description value = description.plan value
  let is_scalar description value = is_scalar (description.plan value)

  let render description renderer placement value =
    Pretty.render renderer placement (description.plan value)

  let inline = Inline
  let start = place
  let finish = finish

  let field description renderer ~last ~name value =
    Pretty.render renderer (Field { last; name }) (description.plan value);
    if not last then Pretty.newline renderer

  let constructor description renderer ~last ~name value =
    Pretty.render renderer (Constructor { last; name }) (description.plan value)

  let constructor_start renderer ~last ~name ~scalar =
    place renderer (Constructor { last; name }) ~scalar

  let variant renderer placement ~polymorphic name =
    let nested = place renderer placement ~scalar:true in
    Pretty.variant renderer ~polymorphic name;
    finish renderer nested

  let variant_label = Pretty.variant
  let empty_record = empty_record
  let json_unit = Json_writer.unit
  let json_bool = Json_writer.bool
  let json_char = Json_writer.char
  let json_int = Json_writer.int
  let json_int32 = Json_writer.int32
  let json_int64 = Json_writer.int64
  let json_float = Json_writer.float
  let json_string = Json_writer.string
  let json_bytes = Json_writer.bytes
  let json_list = Json_writer.list
  let json_array = Json_writer.array
  let json_option = Json_writer.option
  let json_field = write_field
end
