module Local = struct
  type t = int
end

type sample = { value : Local.(t) } [@@deriving observe]
