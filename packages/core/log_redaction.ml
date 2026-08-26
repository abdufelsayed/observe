type redaction_path_step = Field of string | Index of int | Case of string

let copy_string value = Bytes.unsafe_to_string (Bytes.of_string value)

let own_text where value =
  if String.length value > Log_limits.max_string_bytes Log_limits.default then
    invalid_arg ("Observe.Logs.Redaction." ^ where ^ ": value is too large")
  else if Utf8.is_valid value then copy_string value
  else invalid_arg ("Observe.Logs.Redaction." ^ where ^ ": invalid UTF-8")

let own_component where value =
  if String.length value = 0 then
    invalid_arg ("Observe.Logs.Redaction." ^ where ^ ": empty name")
  else own_text where value

let max_path_depth = Log_limits.max_depth Log_limits.default
let max_path_bytes = Log_limits.max_total_bytes Log_limits.default

let path_step_bytes = function
  | Field name -> 4 + String.length (String.escaped name)
  | Index index -> 2 + String.length (string_of_int index)
  | Case name -> 2 + String.length (String.escaped name)

module Path = struct
  type t = {
    steps_rev : redaction_path_step list;
    depth : int;
    encoded_bytes : int;
  }

  let root = { steps_rev = []; depth = 0; encoded_bytes = 1 }

  let extend where step path =
    let depth = path.depth + 1 in
    let step_bytes = path_step_bytes step in
    let encoded_bytes = path.encoded_bytes + step_bytes in
    if depth > max_path_depth then
      invalid_arg ("Observe.Logs.Redaction." ^ where ^ ": path is too deep")
    else if encoded_bytes > max_path_bytes then
      invalid_arg ("Observe.Logs.Redaction." ^ where ^ ": path is too large")
    else { steps_rev = step :: path.steps_rev; depth; encoded_bytes }

  let field name path =
    extend "Path.field" (Field (own_component "Path.field" name)) path

  let fields names =
    List.fold_left (fun path name -> field name path) root names

  let index index path =
    if index < 0 then
      invalid_arg "Observe.Logs.Redaction.Path.index: negative index"
    else extend "Path.index" (Index index) path

  let case name path =
    extend "Path.case" (Case (own_component "Path.case" name)) path

  let steps path = List.rev path.steps_rev
  let encoded_bytes path = path.encoded_bytes

  let to_string path =
    let buffer = Buffer.create 32 in
    Buffer.add_char buffer '$';
    List.iter
      (function
        | Field name ->
            Buffer.add_string buffer "[\"";
            Buffer.add_string buffer (String.escaped name);
            Buffer.add_string buffer "\"]"
        | Index index ->
            Buffer.add_char buffer '[';
            Buffer.add_string buffer (string_of_int index);
            Buffer.add_char buffer ']'
        | Case name ->
            Buffer.add_string buffer "<";
            Buffer.add_string buffer (String.escaped name);
            Buffer.add_char buffer '>')
      (steps path);
    Buffer.contents buffer
end

type matcher_spec =
  | String_equal of string
  | String_prefix of string
  | String_suffix of string
  | String_contains of string
  | Bool of bool
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Float of float
  | Bytes_equal of string
  | Null

module Matcher = struct
  type t = matcher_spec

  let string_equal value = String_equal (own_text "Matcher.string_equal" value)

  let string_prefix value =
    String_prefix (own_text "Matcher.string_prefix" value)

  let string_suffix value =
    String_suffix (own_text "Matcher.string_suffix" value)

  let string_contains value =
    String_contains (own_text "Matcher.string_contains" value)

  let bool value = Bool value
  let int value = Int value
  let int32 value = Int32 value
  let int64 value = Int64 value

  let float value =
    match classify_float value with
    | FP_nan | FP_infinite ->
        invalid_arg "Observe.Logs.Redaction.Matcher.float: non-finite value"
    | FP_normal | FP_subnormal | FP_zero -> Float value

  let bytes_equal value =
    if Bytes.length value > Log_limits.max_bytes_length Log_limits.default then
      invalid_arg
        "Observe.Logs.Redaction.Matcher.bytes_equal: value is too large"
    else Bytes_equal (Bytes.to_string value)

  let null = Null
end

let matcher_is_string = function
  | String_equal _ | String_prefix _ | String_suffix _ | String_contains _ ->
      true
  | Bool _ | Int _ | Int32 _ | Int64 _ | Float _ | Bytes_equal _ | Null -> false

type mask_hidden = Fill of string | Collapse of string

type mask_spec =
  | Keep_prefix of { characters : int; hidden : mask_hidden }
  | Keep_suffix of { characters : int; hidden : mask_hidden }
  | Keep_ends of { characters : int; hidden : mask_hidden }
  | Custom of { fallback : string; apply : string -> string }

module Mask = struct
  type hidden = mask_hidden = Fill of string | Collapse of string
  type t = mask_spec

  let validate_hidden where = function
    | Fill value -> Fill (own_text where value)
    | Collapse value -> Collapse (own_text where value)

  let count where characters =
    if characters < 0 then
      invalid_arg ("Observe.Logs.Redaction." ^ where ^ ": negative count")
    else characters

  let keep_prefix ~characters ~hidden () =
    Keep_prefix
      {
        characters = count "Mask.keep_prefix" characters;
        hidden = validate_hidden "Mask.keep_prefix" hidden;
      }

  let keep_suffix ~characters ~hidden () =
    Keep_suffix
      {
        characters = count "Mask.keep_suffix" characters;
        hidden = validate_hidden "Mask.keep_suffix" hidden;
      }

  let keep_ends ~characters ~hidden () =
    Keep_ends
      {
        characters = count "Mask.keep_ends" characters;
        hidden = validate_hidden "Mask.keep_ends" hidden;
      }

  let custom ?(fallback = "[REDACTED]") apply =
    Custom { fallback = own_text "Mask.custom" fallback; apply }
end

module Action = struct
  type t = Remove | Replace of Value.t | Mask of Mask.t

  let remove = Remove
  let replace value = Replace value
  let mask value = Mask value
end

module Rule = struct
  type t = At of Path.t * Action.t | Matching of Matcher.t * Action.t

  let at path action = At (path, action)
  let matching matcher action = Matching (matcher, action)
end

type error =
  | Conflicting_exact_path of string
  | Conflicting_matcher
  | Invalid_replacement of Snapshot.error
  | Invalid_target of string
  | Policy_limit_exceeded

exception Invalid_redaction of error

type compiled_action =
  | Remove
  | Replace of Snapshot.fragment
  | Mask of mask_spec

type compiled_exact_rule = {
  exact_scope : Schema.identity option;
  exact_path : Path.t;
  exact_action : compiled_action;
}

type compiled_matching_rule = {
  matching_scope : Schema.identity option;
  matching_matcher : matcher_spec;
  matching_action : compiled_action;
}

type compiled_pair = {
  exact_compiled : Snapshot.Redaction.compiled_exact;
  matching_compiled : Snapshot.Redaction.compiled_matching;
}

type t = {
  exact_rules : compiled_exact_rule list;
  matching_rules : compiled_matching_rule list;
  global : compiled_pair;
  typed : (Schema.identity * compiled_pair) list;
}

let snapshot_equal left right =
  let rec equal left right =
    match (Snapshot.view left, Snapshot.view right) with
    | `Null, `Null -> true
    | `Bool left, `Bool right -> Bool.equal left right
    | `Integer left, `Integer right -> left = right
    | `Float left, `Float right -> Float.equal left right
    | `String left, `String right -> String.equal left right
    | `Bytes left, `Bytes right -> String.equal left right
    | `Truncated left, `Truncated right -> left = right
    | `Truncated_list (left, left_reason), `Truncated_list (right, right_reason)
      ->
        left_reason = right_reason && equal_list left right
    | ( `Truncated_object (left, left_reason),
        `Truncated_object (right, right_reason) ) ->
        left_reason = right_reason && equal_fields left right
    | `List left, `List right -> equal_list left right
    | `Object left, `Object right -> equal_fields left right
    | ( `Variant (left_name, left_poly, left_payload),
        `Variant (right_name, right_poly, right_payload) ) ->
        String.equal left_name right_name
        && Bool.equal left_poly right_poly
        && Option.equal equal left_payload right_payload
    | _ -> false
  and equal_list left right =
    match (left, right) with
    | [], [] -> true
    | left :: left_rest, right :: right_rest ->
        equal left right && equal_list left_rest right_rest
    | _ -> false
  and equal_fields left right =
    match (left, right) with
    | [], [] -> true
    | ( (left_name, left_value) :: left_rest,
        (right_name, right_value) :: right_rest ) ->
        String.equal left_name right_name
        && equal left_value right_value
        && equal_fields left_rest right_rest
    | _ -> false
  in
  equal left right

let mask_equal left right =
  match (left, right) with
  | ( Keep_prefix { characters = left_count; hidden = left_hidden },
      Keep_prefix { characters = right_count; hidden = right_hidden } )
  | ( Keep_suffix { characters = left_count; hidden = left_hidden },
      Keep_suffix { characters = right_count; hidden = right_hidden } )
  | ( Keep_ends { characters = left_count; hidden = left_hidden },
      Keep_ends { characters = right_count; hidden = right_hidden } ) ->
      let hidden_equal =
        match (left_hidden, right_hidden) with
        | Fill left, Fill right | Collapse left, Collapse right ->
            String.equal left right
        | Fill _, Collapse _ | Collapse _, Fill _ -> false
      in
      left_count = right_count && hidden_equal
  | ( Custom { fallback = left_fallback; apply = left_apply },
      Custom { fallback = right_fallback; apply = right_apply } ) ->
      String.equal left_fallback right_fallback && left_apply == right_apply
  | Keep_prefix _, (Keep_suffix _ | Keep_ends _ | Custom _)
  | Keep_suffix _, (Keep_prefix _ | Keep_ends _ | Custom _)
  | Keep_ends _, (Keep_prefix _ | Keep_suffix _ | Custom _)
  | Custom _, (Keep_prefix _ | Keep_suffix _ | Keep_ends _) ->
      false

let action_equal left right =
  match (left, right) with
  | Remove, Remove -> true
  | Remove, (Replace _ | Mask _) | (Replace _ | Mask _), Remove -> false
  | Replace left, Replace right ->
      snapshot_equal (Snapshot.complete left) (Snapshot.complete right)
  | Replace _, Mask _ | Mask _, Replace _ -> false
  | Mask left, Mask right -> mask_equal left right

let rec snapshot_has_truncation snapshot =
  match Snapshot.view snapshot with
  | `Truncated _ | `Truncated_list _ | `Truncated_object _ -> true
  | `Null | `Bool _ | `Integer _ | `Float _ | `String _ | `Bytes _ -> false
  | `List values -> List.exists snapshot_has_truncation values
  | `Object fields ->
      List.exists (fun (_, value) -> snapshot_has_truncation value) fields
  | `Variant (_, _, None) -> false
  | `Variant (_, _, Some payload) -> snapshot_has_truncation payload

let path_equal left right = Path.steps left = Path.steps right

let path_is_prefix prefix path =
  let prefix = Path.steps prefix in
  let path = Path.steps path in
  let rec equal prefix path =
    match (prefix, path) with
    | [], _ -> true
    | _ :: _, [] -> false
    | prefix :: prefix_rest, path :: path_rest ->
        prefix = path && equal prefix_rest path_rest
  in
  equal prefix path

let path_is_strict_prefix prefix path =
  path_is_prefix prefix path && not (path_equal prefix path)

let action_is_remove = function Remove -> true | Replace _ | Mask _ -> false

let scope_overlaps left right =
  match (left, right) with
  | None, _ | _, None -> true
  | Some left, Some right -> Schema.same_identity left right

let scope_covers left right =
  match (left, right) with
  | None, _ -> true
  | Some left, Some right -> Schema.same_identity left right
  | Some _, None -> false

let compiled_action = function
  | Action.Remove -> Ok Remove
  | Action.Mask mask -> Ok (Mask mask)
  | Action.Replace value -> (
      try
        match Value.freeze ~limits:Log_limits.default value with
        | Ok replacement
          when snapshot_has_truncation (Snapshot.complete replacement) ->
            Error (Invalid_replacement Snapshot.Limit_exceeded)
        | Ok replacement -> Ok (Replace replacement)
        | Error error -> Error (Invalid_replacement error)
      with
      | (Out_of_memory | Stack_overflow | Sys.Break) as raised -> raise raised
      | _ -> Error (Invalid_replacement Snapshot.Conversion_failed))

let log_shape_steps path =
  List.map
    (function
      | Field name -> Log_shape.Field name
      | Index index -> Log_shape.Index index
      | Case name -> Log_shape.Case name)
    (Path.steps path)

let validate_exact_action path = function
  | Mask _ when Path.steps path = [] ->
      Error (Invalid_target (Path.to_string path))
  | Replace replacement
    when Path.steps path = [] && not (Snapshot.fragment_is_object replacement)
    ->
      Error (Invalid_replacement Snapshot.Unsupported)
  | Remove | Replace _ | Mask _ -> Ok ()

let replacement_is_string replacement =
  match Snapshot.view (Snapshot.complete replacement) with
  | `String _ -> true
  | `Null | `Bool _ | `Integer _ | `Float _ | `Bytes _ | `List _ | `Object _
  | `Variant _ | `Truncated _ | `Truncated_list _ | `Truncated_object _ ->
      false

let validate_matching_action matcher action =
  if matcher_is_string matcher then
    match action with
    | Replace replacement when not (replacement_is_string replacement) ->
        Error (Invalid_replacement Snapshot.Unsupported)
    | Remove | Replace _ | Mask _ -> Ok ()
  else
    match action with
    | Mask _ -> Error (Invalid_target "matching mask requires string values")
    | Remove | Replace _ -> Ok ()

let validate_path ?using path action =
  let steps = Path.steps path in
  let invalid_target () = Error (Invalid_target (Path.to_string path)) in
  if List.length steps > Log_limits.max_depth Log_limits.default then
    invalid_target ()
  else
    let reserved =
      match steps with
      | [] -> false
      | Field name :: _ -> Log_envelope.is_reserved_field name
      | Index _ :: _ | Case _ :: _ -> false
    in
    if reserved then invalid_target ()
    else if steps = [] then Ok ()
    else
      match using with
      | None -> Ok ()
      | Some schema -> (
          match
            Log_shape.lookup (Schema.shape schema) (log_shape_steps path)
          with
          | Log_shape.Known target -> (
              match action with
              | Action.Mask _ when not (Log_shape.accepts_string_mask target) ->
                  invalid_target ()
              | Action.Remove | Action.Replace _ | Action.Mask _ -> Ok ())
          | Log_shape.Empty_case -> (
              match action with
              | Action.Remove -> Ok ()
              | Action.Replace _ | Action.Mask _ -> invalid_target ())
          | Log_shape.Opaque -> Ok ()
          | Log_shape.Missing | Log_shape.Unaddressable -> invalid_target ())

let add_exact (exact : compiled_exact_rule list)
    (candidate : compiled_exact_rule) =
  (* Check every overlapping rule before changing the set.  Looking only at
     the first match makes the result depend on declaration order when a
     global rule and more than one schema-scoped rule are present. *)
  let same_path =
    List.filter
      (fun (existing : compiled_exact_rule) ->
        path_equal existing.exact_path candidate.exact_path
        && scope_overlaps existing.exact_scope candidate.exact_scope)
      exact
  in
  match
    List.find_opt
      (fun (existing : compiled_exact_rule) ->
        not (action_equal existing.exact_action candidate.exact_action))
      same_path
  with
  | Some existing ->
      Error (Conflicting_exact_path (Path.to_string existing.exact_path))
  | None ->
      if
        List.exists
          (fun (existing : compiled_exact_rule) ->
            action_equal existing.exact_action candidate.exact_action
            && scope_covers existing.exact_scope candidate.exact_scope)
          same_path
      then Ok exact
      else
        let exact =
          List.filter
            (fun (existing : compiled_exact_rule) ->
              not
                (path_equal existing.exact_path candidate.exact_path
                && scope_overlaps existing.exact_scope candidate.exact_scope
                && action_equal existing.exact_action candidate.exact_action
                && scope_covers candidate.exact_scope existing.exact_scope))
            exact
        in
        (* A removal at an ancestor makes every descendant rule in the same
           scope redundant.  Other ancestor actions do not subsume anything:
           they may coexist with descendant actions and are resolved by the
           snapshot transformer. *)
        if
          List.exists
            (fun (existing : compiled_exact_rule) ->
              action_is_remove existing.exact_action
              && scope_covers existing.exact_scope candidate.exact_scope
              && path_is_strict_prefix existing.exact_path candidate.exact_path)
            exact
        then Ok exact
        else
          Ok
            (candidate
            :: List.filter
                 (fun (existing : compiled_exact_rule) ->
                   not
                     (action_is_remove candidate.exact_action
                     && scope_covers candidate.exact_scope existing.exact_scope
                     && path_is_strict_prefix candidate.exact_path
                          existing.exact_path))
                 exact)

let matcher_equal left right = left = right

let add_matching (matching : compiled_matching_rule list)
    (candidate : compiled_matching_rule) =
  let same_matcher =
    List.filter
      (fun (existing : compiled_matching_rule) ->
        matcher_equal existing.matching_matcher candidate.matching_matcher
        && scope_overlaps existing.matching_scope candidate.matching_scope)
      matching
  in
  match
    List.find_opt
      (fun (existing : compiled_matching_rule) ->
        not (action_equal existing.matching_action candidate.matching_action))
      same_matcher
  with
  | Some _ -> Error Conflicting_matcher
  | None ->
      if
        List.exists
          (fun (existing : compiled_matching_rule) ->
            scope_covers existing.matching_scope candidate.matching_scope)
          same_matcher
      then Ok matching
      else
        (* A broader equal-action matcher replaces all narrower declarations,
           including every schema-scoped entry it covers.  This preserves the
           global rule's coverage instead of allowing list order to discard it
           or to leave redundant dispatch entries. *)
        Ok
          (candidate
          :: List.filter
               (fun (existing : compiled_matching_rule) ->
                 not
                   (matcher_equal existing.matching_matcher
                      candidate.matching_matcher
                   && scope_overlaps existing.matching_scope
                        candidate.matching_scope
                   && scope_covers candidate.matching_scope
                        existing.matching_scope))
               matching)

let action_rank = function Remove -> 0 | Replace _ -> 1 | Mask _ -> 2

let compare_exact left right =
  let path =
    String.compare
      (Path.to_string left.exact_path)
      (Path.to_string right.exact_path)
  in
  if path <> 0 then path
  else compare (action_rank left.exact_action) (action_rank right.exact_action)

let matcher_key = function
  | String_equal value -> "0:" ^ value
  | String_prefix value -> "1:" ^ value
  | String_suffix value -> "2:" ^ value
  | String_contains value -> "3:" ^ value
  | Bool value -> "4:" ^ string_of_bool value
  | Int value -> "5:" ^ string_of_int value
  | Int32 value -> "6:" ^ Int32.to_string value
  | Int64 value -> "7:" ^ Int64.to_string value
  | Float value -> "8:" ^ Float.to_string value
  | Bytes_equal value -> "a:" ^ String.escaped value
  | Null -> "b:"

let compare_matching left right =
  let matcher =
    String.compare
      (matcher_key left.matching_matcher)
      (matcher_key right.matching_matcher)
  in
  if matcher <> 0 then matcher
  else
    compare
      (action_rank left.matching_action)
      (action_rank right.matching_action)

let max_policy_rules = Log_limits.max_collection_length Log_limits.default
let max_policy_bytes = Log_limits.max_total_bytes Log_limits.default

let action_cost = function
  | Remove -> 1
  | Replace replacement -> 32 + Snapshot.fragment_retained_bytes replacement
  | Mask mask -> (
      match mask with
      | Keep_prefix { hidden; _ }
      | Keep_suffix { hidden; _ }
      | Keep_ends { hidden; _ } -> (
          match hidden with
          | Fill value | Collapse value -> 16 + String.length value)
      | Custom { fallback; _ } -> 24 + String.length fallback)

let matcher_cost = function
  | String_equal value
  | String_prefix value
  | String_suffix value
  | String_contains value
  | Bytes_equal value ->
      16 + String.length value
  | Bool _ | Int _ | Int32 _ | Int64 _ | Float _ | Null -> 16

let rule_cost_exact path action = Path.encoded_bytes path + action_cost action

let rule_cost_matching matcher action =
  matcher_cost matcher + action_cost action

let account_rule ~count ~bytes cost =
  if count >= max_policy_rules || cost > max_policy_bytes - bytes then
    Error Policy_limit_exceeded
  else Ok (count + 1, bytes + cost)

let neutral_path path =
  List.map
    (function
      | Field name -> Snapshot.Redaction.Field name
      | Index index -> Snapshot.Redaction.Index index
      | Case name -> Snapshot.Redaction.Case name)
    (Path.steps path)

let neutral_hidden = function
  | Fill value -> Snapshot.Redaction.Fill value
  | Collapse value -> Snapshot.Redaction.Collapse value

let neutral_mask = function
  | Keep_prefix { characters; hidden } ->
      Snapshot.Redaction.Finite
        (Snapshot.Redaction.Keep_prefix
           { characters; hidden = neutral_hidden hidden })
  | Keep_suffix { characters; hidden } ->
      Snapshot.Redaction.Finite
        (Snapshot.Redaction.Keep_suffix
           { characters; hidden = neutral_hidden hidden })
  | Keep_ends { characters; hidden } ->
      Snapshot.Redaction.Finite
        (Snapshot.Redaction.Keep_ends
           { characters; hidden = neutral_hidden hidden })
  | Custom { fallback; apply } -> Snapshot.Redaction.Custom { fallback; apply }

let neutral_action = function
  | Remove -> Snapshot.Redaction.Remove
  | Replace replacement -> Snapshot.Redaction.Replace replacement
  | Mask mask -> Snapshot.Redaction.Mask (neutral_mask mask)

let neutral_matcher = function
  | String_equal value -> Snapshot.Redaction.String_equal value
  | String_prefix value -> Snapshot.Redaction.String_prefix value
  | String_suffix value -> Snapshot.Redaction.String_suffix value
  | String_contains value -> Snapshot.Redaction.String_contains value
  | Bool value -> Snapshot.Redaction.Bool value
  | Int value -> Snapshot.Redaction.Int value
  | Int32 value -> Snapshot.Redaction.Int32 value
  | Int64 value -> Snapshot.Redaction.Int64 value
  | Float value -> Snapshot.Redaction.Float value
  | Bytes_equal value -> Snapshot.Redaction.Bytes_equal value
  | Null -> Snapshot.Redaction.Null

let neutral_exact_rule (rule : compiled_exact_rule) :
    Snapshot.Redaction.exact_rule =
  let open Snapshot.Redaction in
  {
    path = neutral_path rule.exact_path;
    action = neutral_action rule.exact_action;
  }

let neutral_matching_rule (rule : compiled_matching_rule) :
    Snapshot.Redaction.matching_rule =
  let open Snapshot.Redaction in
  {
    matcher = neutral_matcher rule.matching_matcher;
    action = neutral_action rule.matching_action;
  }

let compile_pair exact matching : compiled_pair =
  {
    exact_compiled =
      Snapshot.Redaction.compile_exact (List.map neutral_exact_rule exact);
    matching_compiled =
      Snapshot.Redaction.compile_matching
        (List.map neutral_matching_rule matching);
  }

let scope_applies scope schema =
  match (scope, schema) with
  | None, _ -> true
  | Some expected, Some actual -> Schema.same_identity expected actual
  | Some _, None -> false

let identities exact matching =
  let add identity identities =
    if
      List.exists
        (fun candidate -> Schema.same_identity candidate identity)
        identities
    then identities
    else identity :: identities
  in
  let identities =
    List.fold_left
      (fun identities (rule : compiled_exact_rule) ->
        match rule.exact_scope with
        | None -> identities
        | Some id -> add id identities)
      [] exact
  in
  List.fold_left
    (fun identities (rule : compiled_matching_rule) ->
      match rule.matching_scope with
      | None -> identities
      | Some id -> add id identities)
    identities matching

let make_policy exact matching =
  let exact = List.sort compare_exact exact in
  let matching = List.sort compare_matching matching in
  let global_exact =
    List.filter
      (fun (rule : compiled_exact_rule) ->
        match rule.exact_scope with None -> true | Some _ -> false)
      exact
  in
  let global_matching =
    List.filter
      (fun (rule : compiled_matching_rule) ->
        match rule.matching_scope with None -> true | Some _ -> false)
      matching
  in
  let global = compile_pair global_exact global_matching in
  let typed =
    List.map
      (fun identity ->
        let exact =
          List.filter
            (fun (rule : compiled_exact_rule) ->
              scope_applies rule.exact_scope (Some identity))
            exact
        in
        let matching =
          List.filter
            (fun (rule : compiled_matching_rule) ->
              scope_applies rule.matching_scope (Some identity))
            matching
        in
        (identity, compile_pair exact matching))
      (identities exact matching)
  in
  { exact_rules = exact; matching_rules = matching; global; typed }

let normalize ?using ~scope rules =
  let action_is_public_remove = function
    | Action.Remove -> true
    | Action.Replace _ | Action.Mask _ -> false
  in
  let rec collect exact matching ~count ~bytes = function
    | [] -> Ok (exact, matching)
    | Rule.At (path, action) :: rest -> (
        match validate_path ?using path action with
        | Error error -> Error error
        | Ok () -> (
            match compiled_action action with
            | Error error -> Error error
            | Ok action -> (
                match validate_exact_action path action with
                | Error error -> Error error
                | Ok () -> (
                    let candidate =
                      {
                        exact_scope = scope;
                        exact_path = path;
                        exact_action = action;
                      }
                    in
                    let cost = rule_cost_exact path action in
                    match account_rule ~count ~bytes cost with
                    | Error error -> Error error
                    | Ok (count, bytes) -> (
                        match add_exact exact candidate with
                        | Error error -> Error error
                        | Ok exact -> collect exact matching ~count ~bytes rest)
                    ))))
    | Rule.Matching (matcher, action) :: rest -> (
        if action_is_public_remove action then
          Error (Invalid_target "matching removal")
        else
          match compiled_action action with
          | Error error -> Error error
          | Ok action -> (
              match validate_matching_action matcher action with
              | Error error -> Error error
              | Ok () -> (
                  let candidate =
                    {
                      matching_scope = scope;
                      matching_matcher = matcher;
                      matching_action = action;
                    }
                  in
                  let cost = rule_cost_matching matcher action in
                  match account_rule ~count ~bytes cost with
                  | Error error -> Error error
                  | Ok (count, bytes) -> (
                      match add_matching matching candidate with
                      | Error error -> Error error
                      | Ok matching -> collect exact matching ~count ~bytes rest
                      ))))
  in
  match collect [] [] ~count:0 ~bytes:0 rules with
  | Error error -> Error error
  | Ok (exact, matching) -> Ok (make_policy exact matching)

let create ?using ~rules () =
  normalize ?using ~scope:(Option.map Schema.identity using) rules

let create_exn ?using ~rules () =
  match create ?using ~rules () with
  | Ok policy -> policy
  | Error error -> raise (Invalid_redaction error)

let combine ~policies () =
  (* [combine] consumes already-owned compiled actions.  It never reconstructs
     a caller [Value.t] or invokes a custom mask.  Re-normalizing the compact
     rule lists is done once at composition time, not while emitting logs. *)
  let rec add_exact_rules exact matching ~count ~bytes = function
    | [] -> Ok (exact, matching, count, bytes)
    | candidate :: rest -> (
        let cost =
          rule_cost_exact candidate.exact_path candidate.exact_action
        in
        match account_rule ~count ~bytes cost with
        | Error error -> Error error
        | Ok (count, bytes) -> (
            match add_exact exact candidate with
            | Error error -> Error error
            | Ok exact -> add_exact_rules exact matching ~count ~bytes rest))
  in
  let rec add_matching_rules exact matching ~count ~bytes = function
    | [] -> Ok (exact, matching, count, bytes)
    | candidate :: rest -> (
        let cost =
          rule_cost_matching candidate.matching_matcher
            candidate.matching_action
        in
        match account_rule ~count ~bytes cost with
        | Error error -> Error error
        | Ok (count, bytes) -> (
            match add_matching matching candidate with
            | Error error -> Error error
            | Ok matching ->
                add_matching_rules exact matching ~count ~bytes rest))
  in
  if List.length policies > max_policy_rules then Error Policy_limit_exceeded
  else
    let rec add_policies exact matching ~count ~bytes = function
      | [] -> Ok (exact, matching)
      | policy :: rest -> (
          match
            add_exact_rules exact matching ~count ~bytes policy.exact_rules
          with
          | Error error -> Error error
          | Ok (exact, matching, count, bytes) -> (
              match
                add_matching_rules exact matching ~count ~bytes
                  policy.matching_rules
              with
              | Error error -> Error error
              | Ok (exact, matching, count, bytes) ->
                  add_policies exact matching ~count ~bytes rest))
    in
    match add_policies [] [] ~count:0 ~bytes:0 policies with
    | Error error -> Error error
    | Ok (exact, matching) -> Ok (make_policy exact matching)

let combine_exn ~policies () =
  match combine ~policies () with
  | Ok policy -> policy
  | Error error -> raise (Invalid_redaction error)

let none = make_policy [] []
let is_none policy = policy.exact_rules = [] && policy.matching_rules = []

let pp_snapshot_error formatter = function
  | Snapshot.Limit_exceeded ->
      Format.pp_print_string formatter "materialization limit exceeded"
  | Snapshot.Invalid_utf8 -> Format.pp_print_string formatter "invalid UTF-8"
  | Snapshot.Duplicate_field ->
      Format.pp_print_string formatter "duplicate field"
  | Snapshot.Unsupported -> Format.pp_print_string formatter "unsupported value"
  | Snapshot.Conversion_failed ->
      Format.pp_print_string formatter "conversion failed"

let pp_error formatter = function
  | Conflicting_exact_path path ->
      Format.fprintf formatter "conflicting redaction actions at %s" path
  | Conflicting_matcher ->
      Format.pp_print_string formatter
        "conflicting redaction actions for one matcher"
  | Invalid_replacement error ->
      Format.fprintf formatter "invalid replacement: %a" pp_snapshot_error error
  | Invalid_target target ->
      Format.fprintf formatter "invalid redaction target %s" target
  | Policy_limit_exceeded ->
      Format.pp_print_string formatter "redaction policy exceeds finite limits"

module Internal = struct
  type nonrec compiled_pair = compiled_pair

  let exact (pair : compiled_pair) = pair.exact_compiled
  let matching (pair : compiled_pair) = pair.matching_compiled

  let compiled policy ~schema =
    match schema with
    | None -> policy.global
    | Some identity -> (
        match
          List.find_opt
            (fun (candidate, _) -> Schema.same_identity candidate identity)
            policy.typed
        with
        | Some (_, compiled) -> compiled
        | None -> policy.global)

  let is_none = is_none
end
