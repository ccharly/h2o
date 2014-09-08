open Printf

let default_attr_builder _ (n, v) =
    sprintf "a_%s %S" n v

let a_type _ (_, v) =
    sprintf "a_type `%s" (String.capitalize v)

let a_class _ (_, v) =
    let v = Str.split (Str.regexp " ") v in
    let v = Mlc_list.enum ~sep:"; " v (sprintf "%S") in
    sprintf "a_class [%s]" v

let a_data _ (n, v) =
    let n = String.sub n 5 ((String.length n) - 5) in
    sprintf "a_user_data %S %S" n v

let attr_specs =
    let s a = `string a in
    let r a = `regexp a in [
        (* Add specializations here *)
        s "class", a_class;
        r "data-.+", a_data;
    ]
