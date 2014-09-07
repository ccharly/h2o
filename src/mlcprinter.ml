open Printf

let print_token t =
    let open Mlcpar in
    printf "::%d::" (Mlcloc.get_current_line ());
    match t with
    | DocType d ->
            printf "DocType:%s\n" d
    | Data d ->
            printf "Data:%s\n" d
    | Tag (n,a) ->
            printf "Tag:%s\n" n;
            List.iter (fun (a,b) -> printf "attr:%s=%s\n" a b) a
    | TagStart (n,a) ->
            printf "TagStart:%s\n" n;
            List.iter (fun (a,b) -> printf "attr:%s=%s\n" a b) a
    | TagEnd (n) ->
            printf "TagEnd:%s\n" n
    | Comment (c) ->
            printf "Comment:%s\n" c
    | EOF ->
            printf "EOF\n"
    | _ ->
            failwith "print_token: unknown token."

module Map = Map.Make(struct type t = string let compare = compare end)

let default_attr_builder = sprintf "a_%s %S"

let attr_specs =
    let s a = `string a in
    let r a = `regexp a in [
        (* Add specializations here *)
        s "class", (fun _ v -> sprintf "a_class [%s]" v);
        r "data-.+",
            (fun k v ->
                sprintf "a_user_data %S %S" k v)
    ]

(* smap = String map, rlist = Str.regexp list *)
let attr_smap, attr_rlist =
    let smap = ref (Map.empty : (string -> string -> string) Map.t) in
    let rlist = ref [] in
    let build (kind, f) = match kind with
    | `string s ->
            smap := Map.add s f !smap
    | `regexp r ->
            rlist := (Str.regexp r, f)::!rlist
    in
    List.iter build attr_specs;
    !smap, !rlist

let print_attr (k, v) =
    try
        let f = Map.find k attr_smap in
        f k v
    with Not_found ->
        try
            let (_,f) =
                List.find
                (fun (r, _) ->
                    Str.string_match r k 0)
                attr_rlist
            in
            f k v
        with Not_found -> default_attr_builder k v

let rec print_ast_node = function
    | `Node (n, a, c) ->
            printf "`Node:%s\n" n;
            List.iter (fun (a,b) -> printf "attr:%s=%s:[%s]\n" a b (print_attr (a,b))) a;
            printf "[\n";
            List.iter print_ast_node c;
            printf "]\n"
    | `Data d ->
            printf "`Data:%s\n" d
    | `Comment c ->
            printf "`Comment:%s\n" c
    | `Eof ->
            printf "`Eof\n"
