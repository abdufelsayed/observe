module String_map = Map.Make (String)
module Int_map = Map.Make (Int)

type resources = {
  nodes : int;
  string_bytes : int;
  byte_bytes : int;
  retained_bytes : int;
}

type integer =
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Decimal of string

type truncation =
  | Depth
  | Object_fields
  | Collection
  | String_bytes
  | Bytes_length
  | Nodes
  | Total_bytes

type indexed_object = {
  order_rev : (int * string) list;
  values : fragment Int_map.t;
  first : int String_map.t;
  next_slot : int;
  resources : resources;
  height : int;
}

and packed_object = {
  fields : (string * fragment) list;
  resources : resources;
  height : int;
}

and object_ =
  | Flat of (string * value) list
  | Single of string * value
  | Packed of packed_object
  | Indexed of indexed_object

and value =
  | Null
  | Bool of bool
  | Integer of integer
  | Float of float
  | String of string
  | Bytes of string
  | Truncated of { reason : truncation; partial : value option }
  | List of value list
  | Object of object_
  | Variant of { name : string; polymorphic : bool; payload : value option }

and fragment = {
  value : value;
  nodes : int;
  string_bytes : int;
  byte_bytes : int;
  retained_bytes : int;
  shape : int;
}

type t = value

type error =
  | Limit_exceeded
  | Invalid_utf8
  | Duplicate_field
  | Unsupported
  | Conversion_failed

type context = {
  limits : Log_limits.t;
  mutable nodes : int;
  mutable string_bytes : int;
  mutable byte_bytes : int;
  mutable retained_bytes : int;
  mutable height : int;
  mutable steps : int;
  mutable last_limit : truncation;
}

(* Charge flat objects for the persistent ordered index they can acquire during
   wide merge. Entering merge therefore cannot invalidate a retained bound. *)
let object_field_retained = 144
let list_entry_retained = 24
let base_node_retained = 32

let no_resources : resources =
  { nodes = 0; string_bytes = 0; byte_bytes = 0; retained_bytes = 0 }

let add_resources (left : resources) (right : resources) : resources =
  {
    nodes = left.nodes + right.nodes;
    string_bytes = left.string_bytes + right.string_bytes;
    byte_bytes = left.byte_bytes + right.byte_bytes;
    retained_bytes = left.retained_bytes + right.retained_bytes;
  }

let replace_resources (total : resources) ~(before : resources)
    ~(after : resources) : resources =
  {
    nodes = total.nodes - before.nodes + after.nodes;
    string_bytes = total.string_bytes - before.string_bytes + after.string_bytes;
    byte_bytes = total.byte_bytes - before.byte_bytes + after.byte_bytes;
    retained_bytes =
      total.retained_bytes - before.retained_bytes + after.retained_bytes;
  }

let resources_of (snapshot : fragment) : resources =
  {
    nodes = snapshot.nodes;
    string_bytes = snapshot.string_bytes;
    byte_bytes = snapshot.byte_bytes;
    retained_bytes = snapshot.retained_bytes;
  }

(* Snapshot height is finitely bounded, so its sign bit is otherwise
   unused. A negative shape stores the complement of the height and marks a
   fragment that still contains wide-merge machinery. Keeping both facts in
   one word avoids making every transient field fragment larger. *)

let shape ~height ~requires_compaction =
  if requires_compaction then lnot height else height

let fragment_height fragment =
  if fragment.shape < 0 then lnot fragment.shape else fragment.shape

let requires_compaction fragment = fragment.shape < 0

let snapshot ?(requires_compaction = false) value (resources : resources) height
    =
  {
    value;
    nodes = resources.nodes;
    string_bytes = resources.string_bytes;
    byte_bytes = resources.byte_bytes;
    retained_bytes = resources.retained_bytes;
    shape = shape ~height ~requires_compaction;
  }

let node_resources ?(string_bytes = 0) ?(byte_bytes = 0) retained_bytes :
    resources =
  { nodes = 1; string_bytes; byte_bytes; retained_bytes }

let create_context ?(limits = Log_limits.default) () =
  {
    limits;
    nodes = 0;
    string_bytes = 0;
    byte_bytes = 0;
    retained_bytes = 0;
    height = 0;
    steps = 0;
    last_limit = Total_bytes;
  }

let limits context = context.limits
let object_field_limit context = Log_limits.max_object_fields context.limits

let collection_length_limit context =
  Log_limits.max_collection_length context.limits

let fail_limit context limit =
  context.last_limit <- limit;
  Error Limit_exceeded

let check_depth context ~depth =
  if depth > Log_limits.max_depth context.limits then fail_limit context Depth
  else Ok ()

let enter context =
  if context.steps >= Log_limits.max_nodes context.limits then
    fail_limit context Nodes
  else (
    context.steps <- context.steps + 1;
    Ok ())

let resources_fit limits (resources : resources) =
  resources.nodes <= Log_limits.max_nodes limits
  && resources.retained_bytes <= Log_limits.max_total_bytes limits

let counts_fit limits ~nodes ~string_bytes:_ ~byte_bytes:_ ~retained_bytes =
  nodes <= Log_limits.max_nodes limits
  && retained_bytes <= Log_limits.max_total_bytes limits

let reserve_counts (context : context) ~depth ~nodes ~string_bytes ~byte_bytes
    ~retained_bytes =
  let limits = context.limits in
  if depth > Log_limits.max_depth limits then fail_limit context Depth
  else if nodes > Log_limits.max_nodes limits then fail_limit context Nodes
  else if retained_bytes > Log_limits.max_total_bytes limits then
    fail_limit context Total_bytes
  else if context.nodes > Log_limits.max_nodes limits - nodes then
    fail_limit context Nodes
  else if
    context.retained_bytes > Log_limits.max_total_bytes limits - retained_bytes
  then fail_limit context Total_bytes
  else (
    context.nodes <- context.nodes + nodes;
    context.string_bytes <- context.string_bytes + string_bytes;
    context.byte_bytes <- context.byte_bytes + byte_bytes;
    context.retained_bytes <- context.retained_bytes + retained_bytes;
    context.height <- max context.height depth;
    Ok ())

let reserve (context : context) ~depth (resources : resources) =
  reserve_counts context ~depth ~nodes:resources.nodes
    ~string_bytes:resources.string_bytes ~byte_bytes:resources.byte_bytes
    ~retained_bytes:resources.retained_bytes

type checkpoint = {
  checkpoint_nodes : int;
  checkpoint_string_bytes : int;
  checkpoint_byte_bytes : int;
  checkpoint_retained_bytes : int;
  checkpoint_height : int;
}

let checkpoint context =
  {
    checkpoint_nodes = context.nodes;
    checkpoint_string_bytes = context.string_bytes;
    checkpoint_byte_bytes = context.byte_bytes;
    checkpoint_retained_bytes = context.retained_bytes;
    checkpoint_height = context.height;
  }

let rollback context checkpoint =
  context.nodes <- checkpoint.checkpoint_nodes;
  context.string_bytes <- checkpoint.checkpoint_string_bytes;
  context.byte_bytes <- checkpoint.checkpoint_byte_bytes;
  context.retained_bytes <- checkpoint.checkpoint_retained_bytes;
  context.height <- checkpoint.checkpoint_height

let copy_string value = Bytes.unsafe_to_string (Bytes.of_string value)
let valid_text = Utf8.is_valid

let own_text value =
  if Utf8.is_valid value then Ok (copy_string value) else Error Invalid_utf8

let leaf context ~depth value ~string_bytes ~byte_bytes ~retained_bytes =
  match
    reserve_counts context ~depth ~nodes:1 ~string_bytes ~byte_bytes
      ~retained_bytes
  with
  | Error _ as error -> error
  | Ok () -> Ok value

let truncated context ~depth reason =
  let depth = min depth (Log_limits.max_depth context.limits) in
  leaf context ~depth
    (Truncated { reason; partial = None })
    ~string_bytes:0 ~byte_bytes:0 ~retained_bytes:base_node_retained

let mark_truncated context ~depth reason partial =
  match
    reserve_counts context ~depth ~nodes:0 ~string_bytes:0 ~byte_bytes:0
      ~retained_bytes:16
  with
  | Error _ as error -> error
  | Ok () -> Ok (Truncated { reason; partial = Some partial })

let localize_apply context ~depth materialize value =
  (* Schema composition needs allocated checkpoints that survive a callback.
     Ordinary recursive materialization only needs one call boundary, so keep
     its rollback state in locals and avoid allocating on every nested value. *)
  let before_nodes = context.nodes in
  let before_string_bytes = context.string_bytes in
  let before_byte_bytes = context.byte_bytes in
  let before_retained_bytes = context.retained_bytes in
  let before_height = context.height in
  match materialize context ~depth value with
  | Ok value -> Ok value
  | Error Limit_exceeded ->
      let reason = context.last_limit in
      context.nodes <- before_nodes;
      context.string_bytes <- before_string_bytes;
      context.byte_bytes <- before_byte_bytes;
      context.retained_bytes <- before_retained_bytes;
      context.height <- before_height;
      truncated context ~depth reason
  | Error _ as error -> error

let truncation = function Truncated { reason; _ } -> Some reason | _ -> None

let null context ~depth =
  leaf context ~depth Null ~string_bytes:0 ~byte_bytes:0
    ~retained_bytes:base_node_retained

let bool context ~depth value =
  leaf context ~depth (Bool value) ~string_bytes:0 ~byte_bytes:0
    ~retained_bytes:base_node_retained

let integer context ~depth value =
  let length = String.length value in
  if length > Log_limits.max_string_bytes context.limits then
    truncated context ~depth String_bytes
  else
    leaf context ~depth
      (Integer (Decimal (copy_string value)))
      ~string_bytes:0 ~byte_bytes:0
      ~retained_bytes:(base_node_retained + length)

let int context ~depth value =
  leaf context ~depth (Integer (Int value)) ~string_bytes:0 ~byte_bytes:0
    ~retained_bytes:(base_node_retained + 8)

let int32 context ~depth value =
  leaf context ~depth (Integer (Int32 value)) ~string_bytes:0 ~byte_bytes:0
    ~retained_bytes:(base_node_retained + 8)

let int64 context ~depth value =
  leaf context ~depth (Integer (Int64 value)) ~string_bytes:0 ~byte_bytes:0
    ~retained_bytes:(base_node_retained + 8)

let float context ~depth value =
  match classify_float value with
  | FP_nan | FP_infinite -> Error Conversion_failed
  | FP_normal | FP_subnormal | FP_zero ->
      leaf context ~depth (Float value) ~string_bytes:0 ~byte_bytes:0
        ~retained_bytes:(base_node_retained + 8)

let copy_text context ~depth value =
  let length = String.length value in
  if length > Log_limits.max_string_bytes context.limits then
    fail_limit context String_bytes
  else if not (Utf8.is_valid value) then Error Invalid_utf8
  else
    match
      reserve_counts context ~depth ~nodes:0 ~string_bytes:length ~byte_bytes:0
        ~retained_bytes:length
    with
    | Error _ as error -> error
    | Ok () -> Ok (copy_string value)

let string context ~depth value =
  let length = String.length value in
  if length > Log_limits.max_string_bytes context.limits then
    truncated context ~depth String_bytes
  else if not (Utf8.is_valid value) then Error Invalid_utf8
  else
    leaf context ~depth
      (String (copy_string value))
      ~string_bytes:length ~byte_bytes:0
      ~retained_bytes:(base_node_retained + length)

let bytes context ~depth value =
  let length = Bytes.length value in
  if length > Log_limits.max_bytes_length context.limits then
    truncated context ~depth Bytes_length
  else
    let copied = Bytes.to_string (Bytes.copy value) in
    if not (Utf8.is_valid copied) then Error Invalid_utf8
    else
      leaf context ~depth (Bytes copied) ~string_bytes:0 ~byte_bytes:length
        ~retained_bytes:(base_node_retained + length)

let width context ~limit ~reason values =
  let rec count total = function
    | [] -> Ok total
    | _ :: rest when total < limit -> count (total + 1) rest
    | _ -> fail_limit context reason
  in
  count 0 values

let rec contains_field_name name = function
  | [] -> false
  | (candidate, _) :: rest ->
      String.equal name candidate || contains_field_name name rest

let duplicate_scan_threshold = 8

let unique_field_names length fields =
  if length <= 1 then true
  else if length <= duplicate_scan_threshold then
    let rec unique = function
      | [] -> true
      | (name, _) :: rest ->
          (not (contains_field_name name rest)) && unique rest
    in
    unique fields
  else
    let names = Hashtbl.create length in
    let rec unique = function
      | [] -> true
      | (name, _) :: rest ->
          if Hashtbl.mem names name then false
          else (
            Hashtbl.add names name ();
            unique rest)
    in
    unique fields

let list context ~depth values =
  match
    width context
      ~limit:(collection_length_limit context)
      ~reason:Collection values
  with
  | Error _ as error -> error
  | Ok length ->
      let own =
        node_resources (base_node_retained + (list_entry_retained * length))
      in
      Result.map (fun () -> List values) (reserve context ~depth own)

let object_single context ~depth name value =
  let length = String.length name in
  if length > Log_limits.max_string_bytes context.limits then
    fail_limit context String_bytes
  else if not (Utf8.is_valid name) then Error Invalid_utf8
  else
    let name = copy_string name in
    match
      reserve_counts context ~depth ~nodes:1 ~string_bytes:length ~byte_bytes:0
        ~retained_bytes:(base_node_retained + object_field_retained + length)
    with
    | Error _ as error -> error
    | Ok () -> Ok (Object (Single (name, value)))

let object_ context ~depth fields =
  match
    width context
      ~limit:(object_field_limit context)
      ~reason:Object_fields fields
  with
  | Error _ as error -> error
  | Ok 1 -> (
      match fields with
      | [ (name, value) ] -> object_single context ~depth name value
      | _ -> assert false)
  | Ok length when not (unique_field_names length fields) ->
      Error Duplicate_field
  | Ok _ -> (
      let rec copy_names string_bytes retained_bytes copied = function
        | [] -> Ok (string_bytes, retained_bytes, List.rev copied)
        | (name, value) :: rest ->
            let length = String.length name in
            if length > Log_limits.max_string_bytes context.limits then
              fail_limit context String_bytes
            else if not (Utf8.is_valid name) then Error Invalid_utf8
            else
              copy_names (string_bytes + length)
                (retained_bytes + object_field_retained + length)
                ((copy_string name, value) :: copied)
                rest
      in
      match copy_names 0 base_node_retained [] fields with
      | Error _ as error -> error
      | Ok (string_bytes, retained_bytes, copied) -> (
          match
            reserve_counts context ~depth ~nodes:1 ~string_bytes ~byte_bytes:0
              ~retained_bytes
          with
          | Error _ as error -> error
          | Ok () -> (
              match copied with
              | [ (name, value) ] -> Ok (Object (Single (name, value)))
              | _ -> Ok (Object (Flat copied)))))

let truncated_object context ~depth reason fields =
  let before = checkpoint context in
  match object_ context ~depth fields with
  | Error _ as error -> error
  | Ok partial -> (
      match mark_truncated context ~depth reason partial with
      | Ok _ as result -> result
      | Error _ as error ->
          rollback context before;
          error)

let rec measure_value value =
  match value with
  | Null | Bool _ | Truncated { partial = None; _ } ->
      snapshot value (node_resources base_node_retained) 0
  | Truncated { partial = Some partial; _ } ->
      let partial = measure_value partial in
      let resources = resources_of partial in
      snapshot value
        { resources with retained_bytes = resources.retained_bytes + 16 }
        (fragment_height partial)
  | Integer (Decimal decimal) ->
      snapshot value
        (node_resources (base_node_retained + String.length decimal))
        0
  | Integer (Int _ | Int32 _ | Int64 _) | Float _ ->
      snapshot value (node_resources (base_node_retained + 8)) 0
  | String text ->
      let length = String.length text in
      snapshot value
        (node_resources ~string_bytes:length (base_node_retained + length))
        0
  | Bytes text ->
      let length = String.length text in
      snapshot value
        (node_resources ~byte_bytes:length (base_node_retained + length))
        0
  | List values ->
      let own =
        node_resources
          (base_node_retained + (list_entry_retained * List.length values))
      in
      let resources, height, needs_compaction =
        List.fold_left
          (fun (resources, height, needs_compaction) value ->
            let child = measure_value value in
            ( add_resources resources (resources_of child),
              max height (fragment_height child + 1),
              needs_compaction || requires_compaction child ))
          (own, 0, false) values
      in
      snapshot ~requires_compaction:needs_compaction value resources height
  | Object (Indexed indexed) ->
      snapshot ~requires_compaction:true value indexed.resources indexed.height
  | Object (Packed packed) ->
      snapshot ~requires_compaction:true value packed.resources packed.height
  | Object (Single (name, child)) ->
      let length = String.length name in
      let child = measure_value child in
      let resources =
        add_resources
          (add_resources
             (node_resources base_node_retained)
             (resources_of child))
          {
            no_resources with
            string_bytes = length;
            retained_bytes = object_field_retained + length;
          }
      in
      snapshot
        ~requires_compaction:(requires_compaction child)
        value resources
        (fragment_height child + 1)
  | Object (Flat fields) ->
      let own =
        List.fold_left
          (fun resources (name, _) ->
            let length = String.length name in
            add_resources resources
              {
                no_resources with
                string_bytes = length;
                retained_bytes = object_field_retained + length;
              })
          (node_resources base_node_retained)
          fields
      in
      let resources, height, needs_compaction =
        List.fold_left
          (fun (resources, height, needs_compaction) (_, value) ->
            let child = measure_value value in
            ( add_resources resources (resources_of child),
              max height (fragment_height child + 1),
              needs_compaction || requires_compaction child ))
          (own, 0, false) fields
      in
      snapshot ~requires_compaction:needs_compaction value resources height
  | Variant { name; payload; _ } -> (
      let length = String.length name in
      let own =
        node_resources ~string_bytes:length (base_node_retained + 16 + length)
      in
      match payload with
      | None -> snapshot value own 0
      | Some payload ->
          let child = measure_value payload in
          snapshot
            ~requires_compaction:(requires_compaction child)
            value
            (add_resources own (resources_of child))
            (fragment_height child + 1))

type readiness = Ready | Stop

type 'entry container = {
  context : context;
  depth : int;
  limit : int;
  mutable count : int;
  mutable entries_rev : 'entry list;
  mutable truncated : truncation option;
  mutable last_nodes : int;
  mutable last_string_bytes : int;
  mutable last_byte_bytes : int;
  mutable last_retained_bytes : int;
  mutable last_height : int;
  mutable pending_name : string;
  mutable name_index : (string, unit) Hashtbl.t option;
}

let create_container context ~depth ~limit =
  match
    reserve_counts context ~depth ~nodes:1 ~string_bytes:0 ~byte_bytes:0
      ~retained_bytes:base_node_retained
  with
  | Error _ as error -> error
  | Ok () ->
      Ok
        {
          context;
          depth;
          limit;
          count = 0;
          entries_rev = [];
          truncated = None;
          last_nodes = context.nodes;
          last_string_bytes = context.string_bytes;
          last_byte_bytes = context.byte_bytes;
          last_retained_bytes = context.retained_bytes;
          last_height = context.height;
          pending_name = "";
          name_index = None;
        }

let remember_before_entry container =
  let context = container.context in
  container.last_nodes <- context.nodes;
  container.last_string_bytes <- context.string_bytes;
  container.last_byte_bytes <- context.byte_bytes;
  container.last_retained_bytes <- context.retained_bytes;
  container.last_height <- context.height

let rollback_last container =
  let context = container.context in
  match container.entries_rev with
  | _ :: rest ->
      context.nodes <- container.last_nodes;
      context.string_bytes <- container.last_string_bytes;
      context.byte_bytes <- container.last_byte_bytes;
      context.retained_bytes <- container.last_retained_bytes;
      context.height <- container.last_height;
      container.entries_rev <- rest;
      container.count <- container.count - 1
  | [] -> ()

let stop container reason =
  container.truncated <- Some reason;
  Stop

let add_entry container entry value =
  container.entries_rev <- entry :: container.entries_rev;
  container.count <- container.count + 1;
  match truncation value with
  | Some (Nodes | Total_bytes) as reason ->
      container.truncated <- reason;
      Stop
  | Some _ | None -> Ready

let finish_container container make_value =
  let partial () = make_value (List.rev container.entries_rev) in
  match container.truncated with
  | None -> Ok (partial ())
  | Some reason -> (
      match
        reserve_counts container.context ~depth:container.depth ~nodes:0
          ~string_bytes:0 ~byte_bytes:0 ~retained_bytes:16
      with
      | Ok () -> Ok (Truncated { reason; partial = Some (partial ()) })
      | Error Limit_exceeded when container.count > 0 -> (
          rollback_last container;
          match
            reserve_counts container.context ~depth:container.depth ~nodes:0
              ~string_bytes:0 ~byte_bytes:0 ~retained_bytes:16
          with
          | Ok () -> Ok (Truncated { reason; partial = Some (partial ()) })
          | Error _ as error -> error)
      | Error _ as error -> error)

module List_builder = struct
  type t = value container

  let create context ~depth =
    create_container context ~depth ~limit:(collection_length_limit context)

  let has_capacity container =
    if Option.is_some container.truncated then false
    else if container.count = container.limit then (
      ignore (stop container Collection : readiness);
      false)
    else true

  let prepare container =
    if not (has_capacity container) then Stop
    else
      let context = container.context in
      remember_before_entry container;
      match
        reserve_counts context ~depth:container.depth ~nodes:0 ~string_bytes:0
          ~byte_bytes:0 ~retained_bytes:list_entry_retained
      with
      | Ok () -> Ready
      | Error Limit_exceeded -> stop container context.last_limit
      | Error _ -> assert false

  let add container value = add_entry container value value
  let truncate container reason = ignore (stop container reason : readiness)
  let finish container = finish_container container (fun values -> List values)
end

module Object_builder = struct
  type t = (string * value) container

  let create context ~depth =
    create_container context ~depth ~limit:(object_field_limit context)

  let has_capacity container =
    if Option.is_some container.truncated then false
    else if container.count = container.limit then (
      ignore (stop container Object_fields : readiness);
      false)
    else true

  let index_names container =
    let index = Hashtbl.create (container.count * 2) in
    let rec add = function
      | [] -> ()
      | (name, _) :: rest ->
          Hashtbl.add index name ();
          add rest
    in
    add container.entries_rev;
    container.name_index <- Some index;
    index

  let duplicate container name =
    match container.name_index with
    | Some index -> Hashtbl.mem index name
    | None when container.count < duplicate_scan_threshold ->
        List.exists
          (fun (candidate, _) -> String.equal candidate name)
          container.entries_rev
    | None -> Hashtbl.mem (index_names container) name

  let prepare_name ~copy container name =
    if not (has_capacity container) then Ok Stop
    else
      let context = container.context in
      let length = String.length name in
      if length > Log_limits.max_string_bytes context.limits then (
        context.last_limit <- String_bytes;
        Ok (stop container String_bytes))
      else if not (Utf8.is_valid name) then Error Invalid_utf8
      else if duplicate container name then Error Duplicate_field
      else (
        remember_before_entry container;
        match
          reserve_counts context ~depth:container.depth ~nodes:0
            ~string_bytes:length ~byte_bytes:0
            ~retained_bytes:(object_field_retained + length)
        with
        | Error Limit_exceeded -> Ok (stop container context.last_limit)
        | Error _ as error -> error
        | Ok () ->
            container.pending_name <- (if copy then copy_string name else name);
            Ok Ready)

  let prepare container name = prepare_name ~copy:true container name
  let prepare_owned container name = prepare_name ~copy:false container name

  let add container value =
    (match container.name_index with
    | None -> ()
    | Some index -> Hashtbl.add index container.pending_name ());
    add_entry container (container.pending_name, value) value

  let truncate container reason = ignore (stop container reason : readiness)

  let finish container =
    finish_container container (function
      | [ (name, value) ] -> Object (Single (name, value))
      | fields -> Object (Flat fields))
end

let object_single_truncated context ~depth reason name value =
  let length = String.length name in
  if length > Log_limits.max_string_bytes context.limits then
    fail_limit context String_bytes
  else if not (Utf8.is_valid name) then Error Invalid_utf8
  else
    let name = copy_string name in
    match
      reserve_counts context ~depth ~nodes:1 ~string_bytes:length ~byte_bytes:0
        ~retained_bytes:
          (base_node_retained + object_field_retained + length + 16)
    with
    | Error _ as error -> error
    | Ok () ->
        Ok
          (Truncated { reason; partial = Some (Object (Single (name, value))) })

let build_object_single context ~depth name materialize =
  let before_nodes = context.nodes in
  let before_string_bytes = context.string_bytes in
  let before_byte_bytes = context.byte_bytes in
  let before_retained_bytes = context.retained_bytes in
  let before_height = context.height in
  match materialize () with
  | Error _ as error -> error
  | Ok value -> (
      let truncated =
        match truncation value with
        | Some (Nodes | Total_bytes) as reason -> reason
        | Some _ | None -> None
      in
      let result =
        match truncated with
        | None -> object_single context ~depth name value
        | Some reason ->
            object_single_truncated context ~depth reason name value
      in
      match result with
      | Ok _ as result -> result
      | Error Limit_exceeded ->
          let reason = context.last_limit in
          context.nodes <- before_nodes;
          context.string_bytes <- before_string_bytes;
          context.byte_bytes <- before_byte_bytes;
          context.retained_bytes <- before_retained_bytes;
          context.height <- before_height;
          truncated_object context ~depth reason []
      | Error _ as error -> error)

let variant context ~depth ~polymorphic name payload =
  let length = String.length name in
  if length > Log_limits.max_string_bytes context.limits then
    truncated context ~depth String_bytes
  else if not (Utf8.is_valid name) then Error Invalid_utf8
  else
    match
      reserve_counts context ~depth ~nodes:1 ~string_bytes:length ~byte_bytes:0
        ~retained_bytes:(base_node_retained + 16 + length)
    with
    | Error _ as error -> error
    | Ok () -> Ok (Variant { name = copy_string name; polymorphic; payload })

let seal (context : context) value =
  {
    value;
    nodes = context.nodes;
    string_bytes = context.string_bytes;
    byte_bytes = context.byte_bytes;
    retained_bytes = context.retained_bytes;
    shape = context.height;
  }

let import context ~depth fragment =
  let height = depth + fragment_height fragment in
  match reserve context ~depth:height (resources_of fragment) with
  | Error _ as error -> error
  | Ok () -> Ok fragment.value

let fragment value = measure_value value

let validate ?(limits = Log_limits.default) (snapshot : fragment) =
  if
    fragment_height snapshot > Log_limits.max_depth limits
    || not (resources_fit limits (resources_of snapshot))
  then Error Limit_exceeded
  else Ok snapshot

let validate_parts limits resources height =
  if
    height > Log_limits.max_depth limits || not (resources_fit limits resources)
  then Error Limit_exceeded
  else Ok ()

let validate_extension ?(limits = Log_limits.default)
    (snapshot : fragment option) ~nodes ~string_bytes ~byte_bytes
    ~retained_bytes =
  let nodes, string_bytes, byte_bytes, retained_bytes =
    match snapshot with
    | None -> (nodes, string_bytes, byte_bytes, retained_bytes)
    | Some snapshot ->
        ( nodes + snapshot.nodes,
          string_bytes + snapshot.string_bytes,
          byte_bytes + snapshot.byte_bytes,
          retained_bytes + snapshot.retained_bytes )
  in
  if counts_fit limits ~nodes ~string_bytes ~byte_bytes ~retained_bytes then
    Ok ()
  else Error Limit_exceeded

let singleton_object_from_owned ?(limits = Log_limits.default) name
    (child : fragment) =
  if not (Utf8.is_valid name) then Error Invalid_utf8
  else
    let name = copy_string name in
    let length = String.length name in
    let resources =
      {
        nodes = 1 + child.nodes;
        string_bytes = child.string_bytes + length;
        byte_bytes = child.byte_bytes;
        retained_bytes =
          base_node_retained
          + child.retained_bytes
          + object_field_retained
          + length;
      }
    in
    let height = fragment_height child + 1 in
    validate ~limits
      (snapshot
         ~requires_compaction:(requires_compaction child)
         (Object (Single (name, child.value)))
         resources height)

let object_from_owned ?(limits = Log_limits.default) fields =
  match fields with
  | [ (name, child) ] -> singleton_object_from_owned ~limits name child
  | _ ->
      let length = List.length fields in
      if length > Log_limits.max_object_fields limits then Error Limit_exceeded
      else if not (unique_field_names length fields) then Error Duplicate_field
      else
        let rec build nodes string_bytes byte_bytes retained_bytes height
            child_requires_compaction copied = function
          | [] ->
              let resources =
                { nodes; string_bytes; byte_bytes; retained_bytes }
              in
              let copied = List.rev copied in
              let object_ =
                match copied with
                | [ (name, child) ] -> Single (name, child.value)
                | fields -> Packed { fields; resources; height }
              in
              validate ~limits
                (snapshot
                   ~requires_compaction:
                     (match object_ with
                     | Single _ -> child_requires_compaction
                     | Packed _ -> true
                     | Flat _ | Indexed _ -> assert false)
                   (Object object_) resources height)
          | (name, (child : fragment)) :: rest ->
              if not (Utf8.is_valid name) then Error Invalid_utf8
              else
                let name = copy_string name in
                let length = String.length name in
                let nodes = nodes + child.nodes in
                let string_bytes = string_bytes + child.string_bytes + length in
                let byte_bytes = byte_bytes + child.byte_bytes in
                let retained_bytes =
                  retained_bytes
                  + child.retained_bytes
                  + object_field_retained
                  + length
                in
                let height = max height (fragment_height child + 1) in
                if
                  counts_fit limits ~nodes ~string_bytes ~byte_bytes
                    ~retained_bytes
                  && height <= Log_limits.max_depth limits
                then
                  build nodes string_bytes byte_bytes retained_bytes height
                    (child_requires_compaction || requires_compaction child)
                    ((name, child) :: copied) rest
                else Error Limit_exceeded
        in
        build 1 0 0 base_node_retained 0 false [] fields

let empty_object =
  let resources = node_resources base_node_retained in
  let packed = { fields = []; resources; height = 0 } in
  snapshot ~requires_compaction:true (Object (Packed packed)) resources 0

let completed_empty_object = Object (Flat [])

let rec fold_indexed indexed callback accumulator = function
  | [] -> accumulator
  | (slot, name) :: rest ->
      let accumulator = fold_indexed indexed callback accumulator rest in
      callback accumulator name (Int_map.find slot indexed.values)

let fold_object_snapshots callback accumulator = function
  | Indexed indexed ->
      fold_indexed indexed callback accumulator indexed.order_rev
  | Packed packed ->
      List.fold_left
        (fun accumulator (name, value) -> callback accumulator name value)
        accumulator packed.fields
  | Single (name, value) -> callback accumulator name (measure_value value)
  | Flat fields ->
      List.fold_left
        (fun accumulator (name, value) ->
          callback accumulator name (measure_value value))
        accumulator fields

let iter_object_values callback = function
  | Flat fields -> List.iter (fun (name, value) -> callback name value) fields
  | Single (name, value) -> callback name value
  | Packed packed ->
      List.iter
        (fun (name, snapshot) -> callback name snapshot.value)
        packed.fields
  | Indexed indexed ->
      ignore
        (fold_indexed indexed
           (fun () name snapshot -> callback name snapshot.value)
           () indexed.order_rev)

let object_length = function
  | Flat fields -> List.length fields
  | Single _ -> 1
  | Packed packed -> List.length packed.fields
  | Indexed indexed -> indexed.next_slot

let index_object = function
  | Indexed indexed -> indexed
  | Single (name, value) ->
      let child = measure_value value in
      let length = String.length name in
      let resources =
        add_resources
          (add_resources
             (node_resources base_node_retained)
             (resources_of child))
          {
            no_resources with
            string_bytes = length;
            retained_bytes = object_field_retained + length;
          }
      in
      {
        order_rev = [ (0, name) ];
        values = Int_map.singleton 0 child;
        first = String_map.singleton name 0;
        next_slot = 1;
        resources;
        height = fragment_height child + 1;
      }
  | Packed packed ->
      List.fold_left
        (fun indexed (name, child) ->
          let slot = indexed.next_slot in
          {
            order_rev = (slot, name) :: indexed.order_rev;
            values = Int_map.add slot child indexed.values;
            first =
              (if String_map.mem name indexed.first then indexed.first
               else String_map.add name slot indexed.first);
            next_slot = slot + 1;
            resources = indexed.resources;
            height = indexed.height;
          })
        {
          order_rev = [];
          values = Int_map.empty;
          first = String_map.empty;
          next_slot = 0;
          resources = packed.resources;
          height = packed.height;
        }
        packed.fields
  | Flat fields ->
      List.fold_left
        (fun indexed (name, value) ->
          let child = measure_value value in
          let slot = indexed.next_slot in
          let length = String.length name in
          {
            order_rev = (slot, name) :: indexed.order_rev;
            values = Int_map.add slot child indexed.values;
            first =
              (if String_map.mem name indexed.first then indexed.first
               else String_map.add name slot indexed.first);
            next_slot = slot + 1;
            resources =
              add_resources
                (add_resources indexed.resources (resources_of child))
                {
                  no_resources with
                  string_bytes = length;
                  retained_bytes = object_field_retained + length;
                };
            height = max indexed.height (fragment_height child + 1);
          })
        {
          order_rev = [];
          values = Int_map.empty;
          first = String_map.empty;
          next_slot = 0;
          resources = node_resources base_node_retained;
          height = 0;
        }
        fields

let pack_object = function
  | Packed packed -> packed
  | Indexed _ -> invalid_arg "Observe.Snapshot.pack_object: indexed object"
  | Single (name, value) ->
      let child = measure_value value in
      let length = String.length name in
      let resources =
        add_resources
          (add_resources
             (node_resources base_node_retained)
             (resources_of child))
          {
            no_resources with
            string_bytes = length;
            retained_bytes = object_field_retained + length;
          }
      in
      {
        fields = [ (name, child) ];
        resources;
        height = fragment_height child + 1;
      }
  | Flat fields ->
      let fields =
        List.map (fun (name, value) -> (name, measure_value value)) fields
      in
      let resources, height =
        List.fold_left
          (fun (resources, height) (name, child) ->
            let length = String.length name in
            ( add_resources
                (add_resources resources (resources_of child))
                {
                  no_resources with
                  string_bytes = length;
                  retained_bytes = object_field_retained + length;
                },
              max height (fragment_height child + 1) ))
          (node_resources base_node_retained, 0)
          fields
      in
      { fields; resources; height }

let packed_index_threshold = 8

let object_fragment = function
  | { value = Object object_; _ } -> Some (object_, None)
  | { value = Truncated { reason; partial = Some (Object object_) }; _ } ->
      Some (object_, Some reason)
  | _ -> None

let is_object_value = function
  | Object _ | Truncated { partial = Some (Object _); _ } -> true
  | Null | Bool _ | Integer _ | Float _ | String _ | Bytes _ | List _
  | Truncated _ | Variant _ ->
      false

let wrap_fragment_truncation ~limits reason fragment =
  match reason with
  | None -> Ok fragment
  | Some reason ->
      let resources = resources_of fragment in
      let resources =
        { resources with retained_bytes = resources.retained_bytes + 16 }
      in
      Result.map
        (fun () ->
          snapshot
            ~requires_compaction:(requires_compaction fragment)
            (Truncated { reason; partial = Some fragment.value })
            resources (fragment_height fragment))
        (validate_parts limits resources (fragment_height fragment))

let rec update_packed ~limits packed name next =
  let rec replace prefix = function
    | [] ->
        if List.length packed.fields >= Log_limits.max_object_fields limits then
          Error Limit_exceeded
        else
          let length = String.length name in
          let resources =
            add_resources
              (add_resources packed.resources (resources_of next))
              {
                no_resources with
                string_bytes = length;
                retained_bytes = object_field_retained + length;
              }
          in
          let height = max packed.height (fragment_height next + 1) in
          let after =
            {
              fields = List.rev_append prefix [ (name, next) ];
              resources;
              height;
            }
          in
          Result.map (fun () -> after) (validate_parts limits resources height)
    | ((field_name, previous) as field) :: rest ->
        if String.equal field_name name then
          Result.bind (merge_value ~limits previous next) (fun merged ->
              let resources =
                replace_resources packed.resources
                  ~before:(resources_of previous) ~after:(resources_of merged)
              in
              let height = max packed.height (fragment_height merged + 1) in
              let after =
                {
                  fields = List.rev_append prefix ((field_name, merged) :: rest);
                  resources;
                  height;
                }
              in
              Result.map
                (fun () -> after)
                (validate_parts limits resources height))
        else replace (field :: prefix) rest
  in
  replace [] packed.fields

and merge_value ~limits previous next =
  match (object_fragment previous, object_fragment next) with
  | Some (previous, previous_truncation), Some (next, next_truncation) ->
      Result.bind (merge_objects ~limits previous next) (fun merged ->
          wrap_fragment_truncation ~limits
            (match next_truncation with
            | Some _ -> next_truncation
            | None -> previous_truncation)
            merged)
  | _ -> Ok next

and merge_objects ~limits previous patch =
  if
    object_length previous <= packed_index_threshold
    && not
         (match previous with
         | Indexed _ -> true
         | Flat _ | Single _ | Packed _ -> false)
  then
    let initial = pack_object previous in
    Result.bind
      (fold_object_snapshots
         (fun result name next ->
           Result.bind result (fun (packed, truncated) ->
               if truncated then Ok (packed, true)
               else if
                 (not (contains_field_name name packed.fields))
                 && List.length packed.fields
                    >= Log_limits.max_object_fields limits
               then Ok (packed, true)
               else
                 Result.map
                   (fun packed -> (packed, false))
                   (update_packed ~limits packed name next)))
         (Ok (initial, false))
         patch)
      (fun (packed, truncated) ->
        let fragment =
          if List.length packed.fields <= packed_index_threshold then
            snapshot ~requires_compaction:true (Object (Packed packed))
              packed.resources packed.height
          else
            let indexed = index_object (Packed packed) in
            snapshot ~requires_compaction:true (Object (Indexed indexed))
              indexed.resources indexed.height
        in
        wrap_fragment_truncation ~limits
          (if truncated then Some Object_fields else None)
          fragment)
  else merge_indexed ~limits previous patch

and merge_indexed ~limits previous patch =
  let initial = index_object previous in
  let update (indexed, truncated) name next =
    if truncated then Ok (indexed, true)
    else if
      (not (String_map.mem name indexed.first))
      && indexed.next_slot >= Log_limits.max_object_fields limits
    then Ok (indexed, true)
    else
      match String_map.find_opt name indexed.first with
      | None ->
          if indexed.next_slot >= Log_limits.max_object_fields limits then
            Error Limit_exceeded
          else
            let slot = indexed.next_slot in
            let length = String.length name in
            let resources =
              add_resources
                (add_resources indexed.resources (resources_of next))
                {
                  no_resources with
                  string_bytes = length;
                  retained_bytes = object_field_retained + length;
                }
            in
            let height = max indexed.height (fragment_height next + 1) in
            let after =
              {
                order_rev = (slot, name) :: indexed.order_rev;
                values = Int_map.add slot next indexed.values;
                first = String_map.add name slot indexed.first;
                next_slot = slot + 1;
                resources;
                height;
              }
            in
            Result.map
              (fun () -> (after, false))
              (validate_parts limits resources height)
      | Some slot ->
          let previous = Int_map.find slot indexed.values in
          Result.bind (merge_value ~limits previous next) (fun merged ->
              let resources =
                replace_resources indexed.resources
                  ~before:(resources_of previous) ~after:(resources_of merged)
              in
              let height = max indexed.height (fragment_height merged + 1) in
              let after =
                {
                  indexed with
                  values = Int_map.add slot merged indexed.values;
                  resources;
                  height;
                }
              in
              Result.map
                (fun () -> (after, false))
                (validate_parts limits resources height))
  in
  Result.bind
    (fold_object_snapshots
       (fun result name next ->
         Result.bind result (fun indexed -> update indexed name next))
       (Ok (initial, false))
       patch)
    (fun (indexed, truncated) ->
      let fragment =
        snapshot ~requires_compaction:true (Object (Indexed indexed))
          indexed.resources indexed.height
      in
      wrap_fragment_truncation ~limits
        (if truncated then Some Object_fields else None)
        fragment)

let merge_object ~limits previous patch =
  match (object_fragment previous, object_fragment patch) with
  | Some (Packed { fields = []; _ }, None), Some _ -> Ok patch
  | ( Some (Single (previous_name, previous_child), previous_truncation),
      Some (Single (patch_name, next_child), next_truncation) )
    when String.equal previous_name patch_name -> (
      let root_truncation =
        match next_truncation with
        | Some _ -> next_truncation
        | None -> previous_truncation
      in
      if is_object_value previous_child && is_object_value next_child then
        let previous_child = fragment previous_child in
        let next_child = fragment next_child in
        Result.bind (merge_value ~limits previous_child next_child)
          (fun merged ->
            Result.bind
              (singleton_object_from_owned ~limits previous_name merged)
              (wrap_fragment_truncation ~limits root_truncation))
      else
        match (next_truncation, previous_truncation) with
        | Some _, _ | None, None -> Ok patch
        | None, Some reason ->
            wrap_fragment_truncation ~limits (Some reason) patch)
  | Some _, Some _ -> merge_value ~limits previous patch
  | _ -> Error Conversion_failed

let fit_object_extension ?(limits = Log_limits.default) (fragment : fragment)
    ~retained_bytes =
  let fits =
    fragment.nodes <= Log_limits.max_nodes limits
    && retained_bytes
       <= Log_limits.max_total_bytes limits - fragment.retained_bytes
  in
  if fits then Ok fragment
  else
    let available = Log_limits.max_total_bytes limits - retained_bytes in
    if available <= 0 then Error Limit_exceeded
    else
      match object_fragment fragment with
      | None -> Error Conversion_failed
      | Some (object_, _) ->
          let fields_rev =
            fold_object_snapshots
              (fun fields name child -> (name, child) :: fields)
              [] object_
          in
          let limits = Log_limits.with_max_total_bytes limits available in
          let rec fit = function
            | [] -> (
                match object_from_owned ~limits [] with
                | Error _ as error -> error
                | Ok fragment ->
                    wrap_fragment_truncation ~limits (Some Total_bytes) fragment
                )
            | _ :: rest as fields_rev -> (
                match object_from_owned ~limits (List.rev fields_rev) with
                | Ok fragment ->
                    wrap_fragment_truncation ~limits (Some Total_bytes) fragment
                | Error Limit_exceeded -> fit rest
                | Error _ as error -> error)
          in
          fit fields_rev

(* Persistent object indexes exist to make repeated wide-event contribution
   cheap.  Once an event is sealed they are dead update machinery, so project
   the snapshot back to the compact immutable value representation before the
   completed log starts its longer publication lifetime.  Strings and scalar
   payloads are already package-owned and remain shared. *)
let rec compact_value = function
  | (Null | Bool _ | Integer _ | Float _ | String _ | Bytes _) as value -> value
  | Truncated ({ partial = None; _ } as truncated) -> Truncated truncated
  | Truncated ({ partial = Some partial; _ } as truncated) ->
      Truncated { truncated with partial = Some (compact_value partial) }
  | List values -> List (List.map compact_value values)
  | Object object_ -> Object (compact_object object_)
  | Variant ({ payload = None; _ } as variant) -> Variant variant
  | Variant ({ payload = Some payload; _ } as variant) ->
      Variant { variant with payload = Some (compact_value payload) }

and compact_fragment_value fragment =
  if requires_compaction fragment then compact_value fragment.value
  else fragment.value

and compact_object = function
  | Single (name, value) -> Single (name, compact_value value)
  | Flat fields ->
      Flat (List.map (fun (name, value) -> (name, compact_value value)) fields)
  | Packed packed -> (
      match packed.fields with
      | [ (name, child) ] -> Single (name, compact_fragment_value child)
      | fields ->
          Flat
            (List.map
               (fun (name, child) -> (name, compact_fragment_value child))
               fields))
  | Indexed indexed ->
      Flat
        (List.rev_map
           (fun (slot, name) ->
             let child = Int_map.find slot indexed.values in
             (name, compact_fragment_value child))
           indexed.order_rev)

let complete fragment =
  if fragment == empty_object then completed_empty_object
  else compact_fragment_value fragment

type view =
  [ `Null
  | `Bool of bool
  | `Integer of integer
  | `Float of float
  | `String of string
  | `Bytes of string
  | `Truncated of truncation
  | `Truncated_list of t list * truncation
  | `Truncated_object of (string * t) list * truncation
  | `List of t list
  | `Object of (string * t) list
  | `Variant of string * bool * t option ]

let object_values = function
  | Flat fields -> fields
  | Single (name, value) -> [ (name, value) ]
  | Packed packed ->
      List.map (fun (name, fragment) -> (name, fragment.value)) packed.fields
  | Indexed indexed ->
      List.rev_map
        (fun (slot, name) -> (name, (Int_map.find slot indexed.values).value))
        indexed.order_rev

let view = function
  | Null -> `Null
  | Bool value -> `Bool value
  | Integer value -> `Integer value
  | Float value -> `Float value
  | String value -> `String value
  | Bytes value -> `Bytes value
  | Truncated { reason; partial = None } -> `Truncated reason
  | Truncated { reason; partial = Some (List values) } ->
      `Truncated_list (values, reason)
  | Truncated { reason; partial = Some (Object fields) } ->
      `Truncated_object (object_values fields, reason)
  | Truncated { reason; partial = Some _ } -> `Truncated reason
  | List values -> `List values
  | Object fields -> `Object (object_values fields)
  | Variant { name; polymorphic; payload } ->
      `Variant (name, polymorphic, payload)

let is_object = function
  | Object _ | Truncated { partial = Some (Object _); _ } -> true
  | _ -> false

let root_field_count = function
  | Object fields -> object_length fields
  | Truncated { partial = Some (Object fields); _ } -> object_length fields
  | _ -> 0

let rec list_has_name_matching predicate = function
  | [] -> false
  | (name, _) :: rest -> predicate name || list_has_name_matching predicate rest

let rec indexed_has_name_matching predicate = function
  | [] -> false
  | (_, name) :: rest ->
      predicate name || indexed_has_name_matching predicate rest

let object_has_name_matching predicate = function
  | Flat fields -> list_has_name_matching predicate fields
  | Single (name, _) -> predicate name
  | Packed packed -> list_has_name_matching predicate packed.fields
  | Indexed indexed -> indexed_has_name_matching predicate indexed.order_rev

let root_has_field_matching predicate = function
  | Object object_ | Truncated { partial = Some (Object object_); _ } ->
      object_has_name_matching predicate object_
  | _ -> false

let fragment_is_object fragment =
  match fragment.value with
  | Object _ | Truncated { partial = Some (Object _); _ } -> true
  | _ -> false

let fragment_root_has_field_matching predicate fragment =
  root_has_field_matching predicate fragment.value

let object_has_field name = function
  | Single (field, _) -> String.equal name field
  | Flat fields -> contains_field_name name fields
  | Packed packed -> contains_field_name name packed.fields
  | Indexed indexed -> String_map.mem name indexed.first

let rec list_names_disjoint object_ = function
  | [] -> true
  | (name, _) :: rest ->
      (not (object_has_field name object_)) && list_names_disjoint object_ rest

let rec indexed_names_disjoint object_ = function
  | [] -> true
  | (_, name) :: rest ->
      (not (object_has_field name object_))
      && indexed_names_disjoint object_ rest

let object_fields_disjoint left = function
  | Flat fields -> list_names_disjoint left fields
  | Single (name, _) -> not (object_has_field name left)
  | Packed packed -> list_names_disjoint left packed.fields
  | Indexed indexed -> indexed_names_disjoint left indexed.order_rev

module Object_accumulator = struct
  type state = fragment

  let empty = empty_object

  let merge ?(limits = Log_limits.default) state patch =
    merge_object ~limits state patch

  let merge_disjoint ?(limits = Log_limits.default) accumulator patch =
    match (object_fragment accumulator, object_fragment patch) with
    | Some (Packed { fields = []; _ }, None), Some _ -> Ok patch
    | Some (left, _), Some (right, _) ->
        if object_fields_disjoint left right then
          merge_object ~limits accumulator patch
        else Error Duplicate_field
    | _, _ -> Error Conversion_failed

  let as_fragment accumulator = accumulator
  let as_value accumulator = accumulator.value
end

type enrichment_origin = Caller | Ordinary | Authority | Conflicted

type enrichment_field = {
  name : string;
  field_value : value option;
  origin : enrichment_origin;
}

let rec equal_value left right =
  match (left, right) with
  | Null, Null -> true
  | Bool left, Bool right -> Bool.equal left right
  | Integer left, Integer right -> left = right
  | Float left, Float right -> Float.equal left right
  | String left, String right | Bytes left, Bytes right ->
      String.equal left right
  | List left, List right -> equal_values left right
  | Object left, Object right ->
      equal_fields (object_values left) (object_values right)
  | ( Truncated { reason = left_reason; partial = left_partial },
      Truncated { reason = right_reason; partial = right_partial } ) ->
      left_reason = right_reason
      && Option.equal equal_value left_partial right_partial
  | ( Variant
        {
          name = left_name;
          polymorphic = left_polymorphic;
          payload = left_payload;
        },
      Variant
        {
          name = right_name;
          polymorphic = right_polymorphic;
          payload = right_payload;
        } ) ->
      String.equal left_name right_name
      && Bool.equal left_polymorphic right_polymorphic
      && Option.equal equal_value left_payload right_payload
  | _, _ -> false

and equal_values left right =
  match (left, right) with
  | [], [] -> true
  | left :: left_rest, right :: right_rest ->
      equal_value left right && equal_values left_rest right_rest
  | _, _ -> false

and equal_fields left right =
  match (left, right) with
  | [], [] -> true
  | (left_name, left) :: left_rest, (right_name, right) :: right_rest ->
      String.equal left_name right_name
      && equal_value left right
      && equal_fields left_rest right_rest
  | _, _ -> false

let object_value = function
  | Object fields -> Some (object_values fields, None)
  | Truncated { reason; partial = Some (Object fields) } ->
      Some (object_values fields, Some reason)
  | _ -> None

let make_object_value fields truncation =
  let object_ =
    match fields with
    | [ (name, value) ] -> Single (name, value)
    | fields -> Flat fields
  in
  match truncation with
  | None -> Object object_
  | Some reason -> Truncated { reason; partial = Some (Object object_) }

(* Enrichment composition is a bounded but potentially repeated merge.  A
   list-only implementation makes every lookup and replacement scan and copy
   the accumulated prefix, turning many small context contributions into a
   quadratic path.  Keep the authored order in a private mutable index while
   composing, then freeze it back to the ordinary immutable field list once.
   The mutable cells never escape this module. *)
type enrichment_slot = { mutable field : enrichment_field }

type enrichment_table = {
  by_name : (string, enrichment_slot) Hashtbl.t;
  mutable order_rev : enrichment_slot list;
  mutable active : int;
}

let enrichment_table fields =
  let table =
    {
      by_name = Hashtbl.create (max 1 (List.length fields));
      order_rev = [];
      active = 0;
    }
  in
  List.iter
    (fun field ->
      let slot = { field } in
      Hashtbl.add table.by_name field.name slot;
      table.order_rev <- slot :: table.order_rev;
      if Option.is_some field.field_value then table.active <- table.active + 1)
    fields;
  table

let find_enrichment_field table name = Hashtbl.find_opt table.by_name name

let replace_enrichment_field table slot replacement =
  let was_active = Option.is_some slot.field.field_value in
  let is_active = Option.is_some replacement.field_value in
  if was_active <> is_active then
    table.active <- (table.active + if is_active then 1 else -1);
  slot.field <- replacement

let append_enrichment_field table field =
  let slot = { field } in
  Hashtbl.add table.by_name field.name slot;
  table.order_rev <- slot :: table.order_rev;
  if Option.is_some field.field_value then table.active <- table.active + 1;
  slot

let remove_appended_enrichment_field table slot =
  match table.order_rev with
  | head :: rest when head == slot ->
      table.order_rev <- rest;
      Hashtbl.remove table.by_name slot.field.name;
      if Option.is_some slot.field.field_value then
        table.active <- table.active - 1
  | _ -> invalid_arg "Observe.Snapshot.remove_appended_enrichment_field"

let active_enrichment_fields table = table.active

let enrichment_table_fields table =
  List.fold_left
    (fun fields slot ->
      match slot.field.field_value with
      | Some value -> (slot.field.name, value) :: fields
      | None -> fields)
    [] table.order_rev

let rec merge_enrichment_value ~limits ~caller left right =
  if equal_value left right then (Some left, false)
  else
    match (object_value left, object_value right) with
    | Some (left, left_truncation), Some (right, right_truncation) ->
        let fields, truncation, conflict =
          merge_enrichment_fields ~limits ~caller left right
        in
        let truncation =
          match right_truncation with
          | Some _ -> right_truncation
          | None -> (
              match left_truncation with
              | Some _ -> left_truncation
              | None -> truncation)
        in
        (Some (make_object_value fields truncation), conflict)
    | _ -> if caller then (Some left, false) else (None, true)

and merge_enrichment_fields ~limits ~caller left right =
  let initial_fields =
    List.map
      (fun (name, value) ->
        {
          name;
          field_value = Some value;
          origin = (if caller then Caller else Ordinary);
        })
      left
  in
  let fields = enrichment_table initial_fields in
  let rec merge truncation conflict = function
    | [] -> (enrichment_table_fields fields, truncation, conflict)
    | (name, next) :: rest -> (
        match find_enrichment_field fields name with
        | None ->
            if
              active_enrichment_fields fields
              >= Log_limits.max_object_fields limits
            then merge (Some Object_fields) conflict rest
            else
              let field =
                { name; field_value = Some next; origin = Ordinary }
              in
              ignore (append_enrichment_field fields field : enrichment_slot);
              merge truncation conflict rest
        | Some slot -> (
            let previous = slot.field in
            match previous.field_value with
            | _ when previous.origin = Conflicted -> merge truncation true rest
            | None ->
                replace_enrichment_field fields slot
                  { previous with origin = Conflicted };
                merge truncation true rest
            | Some previous_value ->
                let previous_is_caller =
                  previous.origin = Caller || previous.origin = Authority
                in
                let merged, value_conflict =
                  merge_enrichment_value ~limits ~caller:previous_is_caller
                    previous_value next
                in
                let origin =
                  match (previous.origin, merged) with
                  | (Caller | Authority), Some _ -> previous.origin
                  | Ordinary, Some _ -> Ordinary
                  | Conflicted, Some _ -> Conflicted
                  | _, None -> Conflicted
                in
                replace_enrichment_field fields slot
                  { previous with field_value = merged; origin };
                merge truncation (conflict || value_conflict) rest))
  in
  merge None false right

let merge_enrichments ~limits ~(caller : fragment)
    (contributions : ((string -> bool) * fragment) list) =
  match object_value caller.value with
  | None -> Error Conversion_failed
  | Some (caller_fields, caller_truncation) ->
      let initial_fields =
        List.map
          (fun (name, value) ->
            { name; field_value = Some value; origin = Caller })
          caller_fields
      in
      let fields = enrichment_table initial_fields in
      let fields_fit () =
        let value = make_object_value (enrichment_table_fields fields) None in
        match validate ~limits (fragment value) with
        | Ok _ -> true
        | Error _ -> false
      in
      let rec merge truncation conflict = function
        | [] ->
            let value =
              make_object_value (enrichment_table_fields fields) truncation
            in
            let fragment = fragment value in
            Result.map
              (fun fragment -> (fragment, conflict))
              (fit_object_extension ~limits fragment ~retained_bytes:0)
        | (is_authoritative, (contribution : fragment)) :: rest -> (
            match object_value contribution.value with
            | None -> Error Conversion_failed
            | Some (contribution_fields, contribution_truncation) ->
                let rec add truncation conflict = function
                  | [] -> merge truncation conflict rest
                  | (name, next) :: contribution_rest -> (
                      let authoritative = is_authoritative name in
                      match find_enrichment_field fields name with
                      | Some slot -> (
                          let previous = slot.field in
                          if authoritative then (
                            replace_enrichment_field fields slot
                              {
                                previous with
                                field_value = Some next;
                                origin = Authority;
                              };
                            if fields_fit () then
                              add truncation conflict contribution_rest
                            else (
                              replace_enrichment_field fields slot previous;
                              add (Some Total_bytes) conflict contribution_rest))
                          else if previous.origin = Authority then
                            add truncation conflict contribution_rest
                          else if previous.origin = Conflicted then
                            add truncation true contribution_rest
                          else
                            match previous.field_value with
                            | Some previous_value ->
                                let merged, value_conflict =
                                  merge_enrichment_value ~limits
                                    ~caller:(previous.origin = Caller)
                                    previous_value next
                                in
                                let origin =
                                  match (previous.origin, merged) with
                                  | Caller, Some _ -> Caller
                                  | Ordinary, Some _ -> Ordinary
                                  | _, Some _ -> previous.origin
                                  | _, None -> Conflicted
                                in
                                replace_enrichment_field fields slot
                                  { previous with field_value = merged; origin };
                                if fields_fit () then
                                  add truncation
                                    (conflict || value_conflict)
                                    contribution_rest
                                else (
                                  replace_enrichment_field fields slot previous;
                                  add (Some Total_bytes)
                                    (conflict || value_conflict)
                                    contribution_rest)
                            | None ->
                                replace_enrichment_field fields slot
                                  { previous with origin = Conflicted };
                                add truncation true contribution_rest)
                      | None ->
                          if
                            active_enrichment_fields fields
                            >= Log_limits.max_object_fields limits
                          then
                            add (Some Object_fields) conflict contribution_rest
                          else
                            let slot =
                              append_enrichment_field fields
                                {
                                  name;
                                  field_value = Some next;
                                  origin =
                                    (if authoritative then Authority
                                     else Ordinary);
                                }
                            in
                            if fields_fit () then
                              add truncation conflict contribution_rest
                            else (
                              remove_appended_enrichment_field fields slot;
                              add (Some Total_bytes) conflict contribution_rest)
                      )
                in
                add
                  (match contribution_truncation with
                  | Some _ -> contribution_truncation
                  | None -> truncation)
                  conflict contribution_fields)
      in
      merge caller_truncation false contributions

let append_integer buffer = function
  | Int value -> Json_writer.int buffer value
  | Int32 value -> Json_writer.int32 buffer value
  | Int64 value -> Json_writer.int64 buffer value
  | Decimal value -> Buffer.add_string buffer value

let truncation_name = function
  | Depth -> "depth"
  | Object_fields -> "object-fields"
  | Collection -> "collection"
  | String_bytes -> "string"
  | Bytes_length -> "bytes"
  | Nodes -> "nodes"
  | Total_bytes -> "size"

let truncation_text reason = "<truncated:" ^ truncation_name reason ^ ">"

let truncation_field_name fields =
  let rec available candidate =
    if object_has_field candidate fields then available (candidate ^ "_")
    else candidate
  in
  available "_observe_truncated"

let rec append_json_value buffer = function
  | Null -> Json_writer.null buffer
  | Bool value -> Json_writer.bool buffer value
  | Integer value -> append_integer buffer value
  | Float value -> Json_writer.float buffer value
  | String value | Bytes value -> Json_writer.trusted_string buffer value
  | Truncated { reason; partial = None } ->
      Json_writer.string buffer (truncation_text reason)
  | Truncated { reason; partial = Some (List values) } ->
      Buffer.add_char buffer '[';
      let first =
        List.fold_left
          (fun first value ->
            if not first then Buffer.add_char buffer ',';
            append_json_value buffer value;
            false)
          true values
      in
      if not first then Buffer.add_char buffer ',';
      Json_writer.string buffer (truncation_text reason);
      Buffer.add_char buffer ']'
  | Truncated { reason; partial = Some (Object fields) } ->
      Buffer.add_char buffer '{';
      let first = append_json_fields buffer true fields in
      ignore
        (append_json_field buffer first
           (truncation_field_name fields)
           (Truncated { reason; partial = None }));
      Buffer.add_char buffer '}'
  | Truncated { reason; partial = Some _ } ->
      Json_writer.string buffer (truncation_text reason)
  | List values ->
      Buffer.add_char buffer '[';
      let rec append first = function
        | [] -> ()
        | value :: rest ->
            if not first then Buffer.add_char buffer ',';
            append_json_value buffer value;
            append false rest
      in
      append true values;
      Buffer.add_char buffer ']'
  | Object fields ->
      Buffer.add_char buffer '{';
      ignore (append_json_fields buffer true fields);
      Buffer.add_char buffer '}'
  | Variant { name; payload = None; _ } ->
      Json_writer.trusted_string buffer name
  | Variant { name; payload = Some payload; _ } ->
      Buffer.add_char buffer '{';
      Json_writer.trusted_name buffer name;
      append_json_value buffer payload;
      Buffer.add_char buffer '}'

and append_json_field buffer first name value =
  if not first then Buffer.add_char buffer ',';
  Json_writer.trusted_name buffer name;
  append_json_value buffer value;
  false

and append_json_fields buffer first = function
  | Flat fields ->
      List.fold_left
        (fun first (name, value) -> append_json_field buffer first name value)
        first fields
  | Single (name, value) -> append_json_field buffer first name value
  | Packed packed ->
      List.fold_left
        (fun first (name, snapshot) ->
          append_json_field buffer first name snapshot.value)
        first packed.fields
  | Indexed indexed ->
      let rec append first = function
        | [] -> first
        | (slot, name) :: rest ->
            let first = append first rest in
            let snapshot = Int_map.find slot indexed.values in
            append_json_field buffer first name snapshot.value
      in
      append first indexed.order_rev

let append_json buffer snapshot = append_json_value buffer snapshot

let append_root_json_fields buffer ~first = function
  | Object fields -> append_json_fields buffer first fields
  | Truncated { reason; partial = Some (Object fields) } ->
      let first = append_json_fields buffer first fields in
      append_json_field buffer first
        (truncation_field_name fields)
        (Truncated { reason; partial = None })
  | Null | Bool _ | Integer _ | Float _ | String _ | Bytes _ | Truncated _
  | List _ | Variant _ ->
      invalid_arg "Snapshot.append_root_json_fields: non-object root"

open Pretty

let pretty_integer renderer = function
  | Int value -> int renderer value
  | Int32 value -> int32 renderer value
  | Int64 value -> int64 renderer value
  | Decimal value -> number renderer value

let rec is_scalar = function
  | Null | Bool _ | Integer _ | Float _ | String _ | Bytes _
  | Truncated { partial = None; _ } ->
      true
  | Truncated { partial = Some _; _ } -> false
  | List values -> List.for_all is_scalar values
  | Object fields -> object_length fields = 0
  | Variant { payload = None; _ } -> true
  | Variant { payload = Some _; _ } -> false

let rec append_scalar renderer = function
  | Null -> null renderer
  | Bool value -> bool renderer value
  | Integer value -> pretty_integer renderer value
  | Float value -> number renderer (Json_writer.float_to_string value)
  | String value | Bytes value -> trusted_string renderer value
  | Truncated { reason; partial = None } ->
      string renderer (truncation_text reason)
  | List values ->
      list_start renderer;
      let rec append = function
        | [] -> ()
        | [ value ] -> append_scalar renderer value
        | value :: rest ->
            append_scalar renderer value;
            list_separator renderer;
            append rest
      in
      append values;
      list_end renderer
  | Object fields when object_length fields = 0 -> empty_record renderer
  | Variant { name; polymorphic; payload = None } ->
      trusted_variant renderer ~polymorphic name
  | Object _
  | Variant { payload = Some _; _ }
  | Truncated { partial = Some _; _ } ->
      assert false

let rec append_pretty_value renderer placement value =
  if is_scalar value then (
    let nested = place renderer placement ~scalar:true in
    append_scalar renderer value;
    finish renderer nested)
  else
    match value with
    | List values ->
        let nested = place renderer placement ~scalar:false in
        let rec append index = function
          | [] -> ()
          | [ value ] ->
              append_pretty_value renderer (Index { last = true; index }) value
          | value :: rest ->
              append_pretty_value renderer (Index { last = false; index }) value;
              newline renderer;
              append (index + 1) rest
        in
        append 0 values;
        finish renderer nested
    | Object fields ->
        let nested = place renderer placement ~scalar:false in
        let last = object_length fields - 1 in
        let index = ref 0 in
        iter_object_values
          (fun name value ->
            let current = !index in
            append_pretty_value renderer
              (Field { last = current = last; name })
              value;
            if current < last then newline renderer;
            index := current + 1)
          fields;
        finish renderer nested
    | Truncated { reason; partial = Some (List values) } ->
        let nested = place renderer placement ~scalar:false in
        let rec append index = function
          | [] ->
              append_pretty_value renderer
                (Index { last = true; index })
                (Truncated { reason; partial = None })
          | value :: rest ->
              append_pretty_value renderer (Index { last = false; index }) value;
              newline renderer;
              append (index + 1) rest
        in
        append 0 values;
        finish renderer nested
    | Truncated { reason; partial = Some (Object fields) } ->
        let nested = place renderer placement ~scalar:false in
        iter_object_values
          (fun name value ->
            append_pretty_value renderer (Field { last = false; name }) value;
            newline renderer)
          fields;
        append_pretty_value renderer
          (Field { last = true; name = truncation_field_name fields })
          (Truncated { reason; partial = None });
        finish renderer nested
    | Truncated { reason; partial = Some _ } ->
        let nested = place renderer placement ~scalar:true in
        string renderer (truncation_text reason);
        finish renderer nested
    | Variant { name; polymorphic; payload = Some payload } ->
        let nested = place renderer placement ~scalar:false in
        let name = if polymorphic then "`" ^ name else name in
        append_pretty_value renderer (Constructor { last = true; name }) payload;
        finish renderer nested
    | Null | Bool _ | Integer _ | Float _ | String _ | Bytes _ | Truncated _
    | Variant { payload = None; _ } ->
        assert false

let append_pretty renderer placement snapshot =
  append_pretty_value renderer placement snapshot

let append_root_pretty_fields renderer ~trailing = function
  | Object fields ->
      let length = object_length fields in
      let index = ref 0 in
      iter_object_values
        (fun name value ->
          let current = !index in
          append_pretty_value renderer
            (Field { last = current = length - 1 && trailing = 0; name })
            value;
          if current < length - 1 then newline renderer;
          index := current + 1)
        fields
  | Truncated { reason; partial = Some (Object fields) } ->
      iter_object_values
        (fun name value ->
          append_pretty_value renderer (Field { last = false; name }) value;
          newline renderer)
        fields;
      append_pretty_value renderer
        (Field { last = trailing = 0; name = truncation_field_name fields })
        (Truncated { reason; partial = None })
  | Null | Bool _ | Integer _ | Float _ | String _ | Bytes _ | Truncated _
  | List _ | Variant _ ->
      invalid_arg "Snapshot.append_root_pretty_fields: non-object root"
