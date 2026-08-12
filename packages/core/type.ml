type 'a t = {
  repr : 'a Repr.t;
  present : 'a -> (Display.t, Display.error) result;
}

let repr description = description.repr
let present description value = description.present value
let of_repr repr = { repr; present = Display.of_repr repr }
let make repr present = { repr; present }
let ( let* ) = Result.bind

let rec map_results convert = function
  | [] -> Ok []
  | value :: rest ->
      let* value = convert value in
      let* rest = map_results convert rest in
      Ok (value :: rest)

let unit = make Repr.unit (fun () -> Ok (Display.Record []))
let bool = make Repr.bool (fun value -> Ok (Display.Bool value))

let char =
  make Repr.char (fun value ->
      let value = String.make 1 value in
      let* value = Display.valid_string value in
      Ok (Display.String value))

let int = make Repr.int (fun value -> Ok (Display.Number (string_of_int value)))

let int32 =
  make Repr.int32 (fun value -> Ok (Display.Number (Int32.to_string value)))

let int63 =
  make Repr.int63 (fun value ->
      Ok (Display.Number (Repr.to_string Repr.int63 value)))

let int64 =
  make Repr.int64 (fun value -> Ok (Display.Number (Int64.to_string value)))

let float =
  make Repr.float (fun value ->
      match classify_float value with
      | FP_nan | FP_infinite -> Error Display.Non_finite_float
      | FP_normal | FP_subnormal | FP_zero ->
          let encoded = string_of_float value in
          let encoded =
            if encoded.[String.length encoded - 1] = '.' then
              String.sub encoded 0 (String.length encoded - 1)
            else encoded
          in
          Ok (Display.Number encoded))

let string =
  make Repr.string (fun value ->
      let* value = Display.valid_string value in
      Ok (Display.String value))

let bytes =
  make Repr.bytes (fun value ->
      let* value = Display.valid_string (Bytes.to_string value) in
      Ok (Display.String value))

type len = Repr.len

let string_of length =
  let description = string in
  { description with repr = Repr.string_of length }

let bytes_of length =
  let description = bytes in
  { description with repr = Repr.bytes_of length }

let boxed description = { description with repr = Repr.boxed description.repr }

let list ?len description =
  make (Repr.list ?len description.repr) (fun values ->
      let* values = map_results description.present values in
      Ok (Display.List values))

let array ?len description =
  make (Repr.array ?len description.repr) (fun values ->
      let* values = map_results description.present (Array.to_list values) in
      Ok (Display.List values))

let option description =
  make (Repr.option description.repr) (function
    | None -> Ok Display.Null
    | Some value -> description.present value)

let pair left right =
  make (Repr.pair left.repr right.repr) (fun (left_value, right_value) ->
      let* left_value = left.present left_value in
      let* right_value = right.present right_value in
      Ok (Display.List [ left_value; right_value ]))

let triple first second third =
  make (Repr.triple first.repr second.repr third.repr)
    (fun (first_value, second_value, third_value) ->
      let* first_value = first.present first_value in
      let* second_value = second.present second_value in
      let* third_value = third.present third_value in
      Ok (Display.List [ first_value; second_value; third_value ]))

let quad first second third fourth =
  make (Repr.quad first.repr second.repr third.repr fourth.repr)
    (fun (first_value, second_value, third_value, fourth_value) ->
      let* first_value = first.present first_value in
      let* second_value = second.present second_value in
      let* third_value = third.present third_value in
      let* fourth_value = fourth.present fourth_value in
      Ok (Display.List [ first_value; second_value; third_value; fourth_value ]))

let variant_display ?(polymorphic = false) name payload =
  Display.Variant { name; polymorphic; payload }

let result ok error =
  make (Repr.result ok.repr error.repr) (function
    | Ok value ->
        let* value = ok.present value in
        Ok (variant_display "Ok" (Some value))
    | Error value ->
        let* value = error.present value in
        Ok (variant_display "Error" (Some value)))

let seq description =
  make (Repr.seq description.repr) (fun values ->
      let* values = map_results description.present (List.of_seq values) in
      Ok (Display.List values))

let ref description =
  make (Repr.ref description.repr) (fun value -> description.present !value)

let lazy_t description =
  make (Repr.lazy_t description.repr) (fun value ->
      description.present (Lazy.force value))

let queue description =
  make (Repr.queue description.repr) (fun values ->
      let* values =
        map_results description.present (List.of_seq (Queue.to_seq values))
      in
      Ok (Display.List values))

let stack description =
  make (Repr.stack description.repr) (fun values ->
      let* values =
        map_results description.present (List.of_seq (Stack.to_seq values))
      in
      Ok (Display.List values))

let hashtbl key value =
  make (Repr.hashtbl key.repr value.repr) (fun table ->
      let entries = Hashtbl.to_seq table |> List.of_seq in
      let convert (key_value, value_value) =
        let* key_value = key.present key_value in
        let* value_value = value.present value_value in
        Ok (Display.List [ key_value; value_value ])
      in
      let* entries = map_results convert entries in
      Ok (Display.List entries))

type empty = Repr.empty = |

let empty = of_repr Repr.empty

type ('a, 'b, 'c) open_record = ('a, 'b, 'c) Repr.open_record
type ('a, 'b) field = ('a, 'b) Repr.field

let record = Repr.record
let field name description getter = Repr.field name description.repr getter
let ( |+ ) = Repr.( |+ )
let sealr record = of_repr (Repr.sealr record)

type ('a, 'b, 'c) open_variant = ('a, 'b, 'c) Repr.open_variant
type ('a, 'b) case = ('a, 'b) Repr.case
type 'a case_p = 'a Repr.case_p

let variant = Repr.variant
let case0 = Repr.case0
let case1 name description inject = Repr.case1 name description.repr inject
let ( |~ ) = Repr.( |~ )
let sealv variant = of_repr (Repr.sealv variant)

let enum name cases =
  let repr = Repr.enum name cases in
  let equal = Repr.unstage (Repr.equal repr) in
  make repr (fun value ->
      match
        List.find_opt (fun (_, candidate) -> equal value candidate) cases
      with
      | Some (name, _) -> Ok (variant_display name None)
      | None -> Error Display.Unsupported_value)

let mu make_description =
  of_repr
    (Repr.mu (fun machine ->
         let description = of_repr machine in
         repr (make_description description)))

let mu2 make_descriptions =
  let left, right =
    Repr.mu2 (fun left right ->
        let left, right = make_descriptions (of_repr left) (of_repr right) in
        (left.repr, right.repr))
  in
  (of_repr left, of_repr right)

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

let to_json_string ?minify description =
  Repr.to_json_string ?minify description.repr

let of_json_string description = Repr.of_json_string description.repr
let encode_bin description = Repr.encode_bin description.repr
let decode_bin description = Repr.decode_bin description.repr
let to_bin_string description = Repr.to_bin_string description.repr
let of_bin_string description = Repr.of_bin_string description.repr
let size_of description = Repr.size_of description.repr

let like ?pp ?of_string ?json ?bin ?unboxed_bin ?equal ?compare ?short_hash
    ?pre_hash description =
  {
    description with
    repr =
      Repr.like ?pp ?of_string ?json ?bin ?unboxed_bin ?equal ?compare
        ?short_hash ?pre_hash description.repr;
  }

let partially_abstract ~pp ~of_string ~json ~bin ~unboxed_bin ~equal ~compare
    ~short_hash ~pre_hash description =
  {
    description with
    repr =
      Repr.partially_abstract ~pp ~of_string ~json ~bin ~unboxed_bin ~equal
        ~compare ~short_hash ~pre_hash description.repr;
  }

let map ?pp ?of_string ?json ?bin ?unboxed_bin ?equal ?compare ?short_hash
    ?pre_hash description decode encode =
  make
    (Repr.map ?pp ?of_string ?json ?bin ?unboxed_bin ?equal ?compare ?short_hash
       ?pre_hash description.repr decode encode) (fun value ->
      description.present (encode value))

module For_ppx = struct
  type view = Display.t
  type error = Display.error
  type result = (view, error) Stdlib.result

  let with_present description present = { description with present }
  let present = present

  let record fields =
    let convert (name, value) =
      let* name = Display.valid_string name in
      let* value = value in
      Ok (name, value)
    in
    let* fields = map_results convert fields in
    Ok (Display.Record fields)

  let list values =
    let* values = map_results Fun.id values in
    Ok (Display.List values)

  let list_map present values =
    let* values = map_results present values in
    Ok (Display.List values)

  let option = function None -> Ok Display.Null | Some value -> value

  let variant ~polymorphic name payload =
    let* name = Display.valid_string name in
    let* payload =
      match payload with
      | None -> Ok None
      | Some payload -> Result.map Option.some payload
    in
    Ok (Display.Variant { name; polymorphic; payload })
end
