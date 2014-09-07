open Printf

let a_class k v =
    let v = Str.split (Str.regexp " ") v in
    let v = List.fold_left (sprintf "%s%S; ") "" v in
    sprintf "a_class [%s]" v

let a_data k v =
    let k = String.sub k 5 (String.length k) in
    sprintf "a_user_data %S %S" k v

let attr_specs =
    let s a = `string a in
    let r a = `regexp a in [
        (* Add specializations here *)
        s "class", a_class;
        r "data-.+", a_data;
    ]
