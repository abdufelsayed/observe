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
  mutable nodes : int;
  mutable string_bytes : int;
  mutable byte_bytes : int;
  mutable retained_bytes : int;
  mutable height : int;
}

let max_depth = 64
let width_limit = 1_024
let max_nodes = 100_000
let max_string_bytes = 1_048_576
let max_byte_bytes = 1_048_576
let max_retained_bytes = 4_194_304

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

(* Snapshot height is bounded to [max_depth], so its sign bit is otherwise
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

let create_context () =
  {
    nodes = 0;
    string_bytes = 0;
    byte_bytes = 0;
    retained_bytes = 0;
    height = 0;
  }

let check_depth ~depth =
  if depth > max_depth then Error Limit_exceeded else Ok ()

let resources_fit (resources : resources) =
  resources.nodes <= max_nodes
  && resources.string_bytes <= max_string_bytes
  && resources.byte_bytes <= max_byte_bytes
  && resources.retained_bytes <= max_retained_bytes

let counts_fit ~nodes ~string_bytes ~byte_bytes ~retained_bytes =
  nodes <= max_nodes
  && string_bytes <= max_string_bytes
  && byte_bytes <= max_byte_bytes
  && retained_bytes <= max_retained_bytes

let reserve_counts (context : context) ~depth ~nodes ~string_bytes ~byte_bytes
    ~retained_bytes =
  if
    depth > max_depth
    || not (counts_fit ~nodes ~string_bytes ~byte_bytes ~retained_bytes)
  then Error Limit_exceeded
  else if
    context.nodes > max_nodes - nodes
    || context.string_bytes > max_string_bytes - string_bytes
    || context.byte_bytes > max_byte_bytes - byte_bytes
    || context.retained_bytes > max_retained_bytes - retained_bytes
  then Error Limit_exceeded
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

let null context ~depth =
  leaf context ~depth Null ~string_bytes:0 ~byte_bytes:0
    ~retained_bytes:base_node_retained

let bool context ~depth value =
  leaf context ~depth (Bool value) ~string_bytes:0 ~byte_bytes:0
    ~retained_bytes:base_node_retained

let integer context ~depth value =
  let length = String.length value in
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
  if not (Utf8.is_valid value) then Error Invalid_utf8
  else
    match
      reserve_counts context ~depth ~nodes:0 ~string_bytes:length ~byte_bytes:0
        ~retained_bytes:length
    with
    | Error _ as error -> error
    | Ok () -> Ok (copy_string value)

let string context ~depth value =
  let length = String.length value in
  if not (Utf8.is_valid value) then Error Invalid_utf8
  else
    leaf context ~depth
      (String (copy_string value))
      ~string_bytes:length ~byte_bytes:0
      ~retained_bytes:(base_node_retained + length)

let bytes context ~depth value =
  let length = Bytes.length value in
  let copied = Bytes.to_string (Bytes.copy value) in
  if not (Utf8.is_valid copied) then Error Invalid_utf8
  else
    leaf context ~depth (Bytes copied) ~string_bytes:0 ~byte_bytes:length
      ~retained_bytes:(base_node_retained + length)

let width values =
  let rec count total = function
    | [] -> Ok total
    | _ :: rest when total < width_limit -> count (total + 1) rest
    | _ -> Error Limit_exceeded
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
  match width values with
  | Error _ as error -> error
  | Ok length ->
      let own =
        node_resources (base_node_retained + (list_entry_retained * length))
      in
      Result.map (fun () -> List values) (reserve context ~depth own)

let object_ context ~depth fields =
  match width fields with
  | Error _ as error -> error
  | Ok length when not (unique_field_names length fields) ->
      Error Duplicate_field
  | Ok _ -> (
      let rec copy_names string_bytes retained_bytes copied = function
        | [] -> Ok (string_bytes, retained_bytes, List.rev copied)
        | (name, value) :: rest ->
            if not (Utf8.is_valid name) then Error Invalid_utf8
            else
              let length = String.length name in
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

let object_with_owned_names context ~depth fields =
  match width fields with
  | Error _ as error -> error
  | Ok length when not (unique_field_names length fields) ->
      Error Duplicate_field
  | Ok _ -> (
      let string_bytes, retained_bytes =
        List.fold_left
          (fun (string_bytes, retained_bytes) (name, _) ->
            let length = String.length name in
            ( string_bytes + length,
              retained_bytes + object_field_retained + length ))
          (0, base_node_retained) fields
      in
      match
        reserve_counts context ~depth ~nodes:1 ~string_bytes ~byte_bytes:0
          ~retained_bytes
      with
      | Error _ as error -> error
      | Ok () -> (
          match fields with
          | [ (name, value) ] -> Ok (Object (Single (name, value)))
          | _ -> Ok (Object (Flat fields))))

let variant context ~depth ~polymorphic name payload =
  let length = String.length name in
  if not (Utf8.is_valid name) then Error Invalid_utf8
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

let rec measure_value value =
  match value with
  | Null | Bool _ -> snapshot value (node_resources base_node_retained) 0
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

let validate (snapshot : fragment) =
  if
    fragment_height snapshot > max_depth
    || not (resources_fit (resources_of snapshot))
  then Error Limit_exceeded
  else Ok snapshot

let validate_parts resources height =
  if height > max_depth || not (resources_fit resources) then
    Error Limit_exceeded
  else Ok ()

let validate_extension (snapshot : fragment option) ~nodes ~string_bytes
    ~byte_bytes ~retained_bytes =
  let nodes, string_bytes, byte_bytes, retained_bytes =
    match snapshot with
    | None -> (nodes, string_bytes, byte_bytes, retained_bytes)
    | Some snapshot ->
        ( nodes + snapshot.nodes,
          string_bytes + snapshot.string_bytes,
          byte_bytes + snapshot.byte_bytes,
          retained_bytes + snapshot.retained_bytes )
  in
  if counts_fit ~nodes ~string_bytes ~byte_bytes ~retained_bytes then Ok ()
  else Error Limit_exceeded

let singleton_object_from_owned name (child : fragment) =
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
    validate
      (snapshot
         ~requires_compaction:(requires_compaction child)
         (Object (Single (name, child.value)))
         resources height)

let object_from_owned fields =
  match fields with
  | [ (name, child) ] -> singleton_object_from_owned name child
  | _ -> (
      match width fields with
      | Error _ as error -> error
      | Ok length when not (unique_field_names length fields) ->
          Error Duplicate_field
      | Ok _ ->
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
                validate
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
                  let string_bytes =
                    string_bytes + child.string_bytes + length
                  in
                  let byte_bytes = byte_bytes + child.byte_bytes in
                  let retained_bytes =
                    retained_bytes
                    + child.retained_bytes
                    + object_field_retained
                    + length
                  in
                  let height = max height (fragment_height child + 1) in
                  if
                    counts_fit ~nodes ~string_bytes ~byte_bytes ~retained_bytes
                    && height <= max_depth
                  then
                    build nodes string_bytes byte_bytes retained_bytes height
                      (child_requires_compaction || requires_compaction child)
                      ((name, child) :: copied) rest
                  else Error Limit_exceeded
          in
          build 1 0 0 base_node_retained 0 false [] fields)

let empty_object =
  let resources = node_resources base_node_retained in
  let packed = { fields = []; resources; height = 0 } in
  snapshot ~requires_compaction:true (Object (Packed packed)) resources 0

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

let rec update_packed packed name next =
  let rec replace prefix = function
    | [] ->
        if List.length packed.fields >= width_limit then Error Limit_exceeded
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
          Result.map (fun () -> after) (validate_parts resources height)
    | ((field_name, previous) as field) :: rest ->
        if String.equal field_name name then
          Result.bind (merge_value previous next) (fun merged ->
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
              Result.map (fun () -> after) (validate_parts resources height))
        else replace (field :: prefix) rest
  in
  replace [] packed.fields

and merge_value previous next =
  match (previous.value, next.value) with
  | Object previous, Object next -> merge_objects previous next
  | _, _ -> Ok next

and merge_objects previous patch =
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
           Result.bind result (fun packed -> update_packed packed name next))
         (Ok initial) patch)
      (fun packed ->
        if List.length packed.fields <= packed_index_threshold then
          Ok
            (snapshot ~requires_compaction:true (Object (Packed packed))
               packed.resources packed.height)
        else
          let indexed = index_object (Packed packed) in
          Ok
            (snapshot ~requires_compaction:true (Object (Indexed indexed))
               indexed.resources indexed.height))
  else merge_indexed previous patch

and merge_indexed previous patch =
  let initial = index_object previous in
  let update indexed name next =
    match String_map.find_opt name indexed.first with
    | None ->
        if indexed.next_slot >= width_limit then Error Limit_exceeded
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
          Result.map (fun () -> after) (validate_parts resources height)
    | Some slot ->
        let previous = Int_map.find slot indexed.values in
        Result.bind (merge_value previous next) (fun merged ->
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
            Result.map (fun () -> after) (validate_parts resources height))
  in
  Result.map
    (fun indexed ->
      snapshot ~requires_compaction:true (Object (Indexed indexed))
        indexed.resources indexed.height)
    (fold_object_snapshots
       (fun result name next ->
         Result.bind result (fun indexed -> update indexed name next))
       (Ok initial) patch)

let merge_object previous patch =
  match (previous.value, patch.value) with
  | Object (Packed { fields = []; _ }), Object _ -> Ok patch
  | ( Object (Single (previous_name, previous_child)),
      Object (Single (patch_name, next_child)) )
    when String.equal previous_name patch_name -> (
      match (previous_child, next_child) with
      | Object _, Object _ ->
          merge_objects
            (Single (previous_name, previous_child))
            (Single (patch_name, next_child))
      | _, _ -> Ok patch)
  | ( Object
        (Packed
           ({ fields = [ (previous_name, previous_child) ]; _ } as previous)),
      Object (Packed { fields = [ (patch_name, next_child) ]; _ }) )
    when String.equal previous_name patch_name ->
      Result.bind (merge_value previous_child next_child) (fun merged ->
          let resources =
            {
              nodes =
                previous.resources.nodes - previous_child.nodes + merged.nodes;
              string_bytes =
                previous.resources.string_bytes
                - previous_child.string_bytes
                + merged.string_bytes;
              byte_bytes =
                previous.resources.byte_bytes
                - previous_child.byte_bytes
                + merged.byte_bytes;
              retained_bytes =
                previous.resources.retained_bytes
                - previous_child.retained_bytes
                + merged.retained_bytes;
            }
          in
          let height = max previous.height (fragment_height merged + 1) in
          let packed =
            { fields = [ (previous_name, merged) ]; resources; height }
          in
          Result.map
            (fun () ->
              snapshot ~requires_compaction:true (Object (Packed packed))
                resources height)
            (validate_parts resources height))
  | Object previous, Object patch -> merge_objects previous patch
  | _, _ -> Error Conversion_failed

(* Persistent object indexes exist to make repeated wide-event contribution
   cheap.  Once an event is sealed they are dead update machinery, so project
   the snapshot back to the compact immutable value representation before the
   completed log starts its longer publication lifetime.  Strings and scalar
   payloads are already package-owned and remain shared. *)
let rec compact_value = function
  | (Null | Bool _ | Integer _ | Float _ | String _ | Bytes _) as value -> value
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

let complete fragment = compact_fragment_value fragment
let is_object = function Object _ -> true | _ -> false

let root_field_count = function
  | Object fields -> object_length fields
  | Null | Bool _ | Integer _ | Float _ | String _ | Bytes _ | List _
  | Variant _ ->
      0

let root_has_field_matching predicate snapshot =
  match snapshot with
  | Object fields ->
      let found = ref false in
      iter_object_values
        (fun name _ -> if predicate name then found := true)
        fields;
      !found
  | Null | Bool _ | Integer _ | Float _ | String _ | Bytes _ | List _
  | Variant _ ->
      false

let object_has_field name = function
  | Single (field, _) -> String.equal name field
  | Flat fields -> contains_field_name name fields
  | Packed packed -> contains_field_name name packed.fields
  | Indexed indexed -> String_map.mem name indexed.first

let object_fields_disjoint left right =
  let disjoint = ref true in
  iter_object_values
    (fun name _ -> if object_has_field name left then disjoint := false)
    right;
  !disjoint

module Object_accumulator = struct
  type state = fragment

  let empty = empty_object
  let merge = merge_object

  let merge_disjoint accumulator patch =
    match (accumulator.value, patch.value) with
    | Object (Packed { fields = []; _ }), Object _ -> Ok patch
    | Object left, Object right ->
        if object_fields_disjoint left right then merge_object accumulator patch
        else Error Duplicate_field
    | _, _ -> Error Conversion_failed

  let as_fragment accumulator = accumulator
end

let append_integer buffer = function
  | Int value -> Json_writer.int buffer value
  | Int32 value -> Json_writer.int32 buffer value
  | Int64 value -> Json_writer.int64 buffer value
  | Decimal value -> Buffer.add_string buffer value

let rec append_json_value buffer = function
  | Null -> Json_writer.null buffer
  | Bool value -> Json_writer.bool buffer value
  | Integer value -> append_integer buffer value
  | Float value -> Json_writer.float buffer value
  | String value | Bytes value -> Json_writer.trusted_string buffer value
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
  | Null | Bool _ | Integer _ | Float _ | String _ | Bytes _ | List _
  | Variant _ ->
      invalid_arg "Snapshot.append_root_json_fields: non-object root"

open Pretty

let pretty_integer renderer = function
  | Int value -> int renderer value
  | Int32 value -> int32 renderer value
  | Int64 value -> int64 renderer value
  | Decimal value -> number renderer value

let rec is_scalar = function
  | Null | Bool _ | Integer _ | Float _ | String _ | Bytes _ -> true
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
  | Object _ | Variant { payload = Some _; _ } -> assert false

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
    | Variant { name; polymorphic; payload = Some payload } ->
        let nested = place renderer placement ~scalar:false in
        let name = if polymorphic then "`" ^ name else name in
        append_pretty_value renderer (Constructor { last = true; name }) payload;
        finish renderer nested
    | Null | Bool _ | Integer _ | Float _ | String _ | Bytes _
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
  | Null | Bool _ | Integer _ | Float _ | String _ | Bytes _ | List _
  | Variant _ ->
      invalid_arg "Snapshot.append_root_pretty_fields: non-object root"
