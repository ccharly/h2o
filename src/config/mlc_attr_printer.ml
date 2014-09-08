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

module C = Mlc_cache_builder.Make(struct
    type value = (string * string)

    let has_name _ = true
    let get_name (n, _) = n
    let default = default_attr_builder
end)

let () =
    let open C in
    List.iter C.register [
        (* Add specializations here *)
        s "type", a_type;
        s "class", a_class;
        r "data-.+", a_data;
    ]

let build = C.build
let print_attr v =
    printf "%s\n" (build v)
