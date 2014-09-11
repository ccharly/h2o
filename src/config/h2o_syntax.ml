open Printf

let depth = ref 0
let pad_coef = 2

let incr_depth () = incr depth
let decr_depth () = decr depth

let make_pad () =
    let rec make_pad = function
        | n when n <= 0 -> ""
        | n -> " "^(make_pad (n-1))
    in
    make_pad (!depth * pad_coef)

let make_label ?(is_list = false) n v =
    if is_list
    then sprintf "~%s:[%s]" n v
    else sprintf "~%s:%s" n v

let make_unit () =
    "()"
