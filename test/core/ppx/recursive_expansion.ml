type node = Leaf of string | Branch of node list [@@deriving observe]
