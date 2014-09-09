type t = [
    | `Node of (string * (string * string) list * t list)
    | `Data of string
    | `Comment of string
    | `Eof
]


