open Printf

let print_token t =
    let open H2o_par in
    printf "::%d::" (H2o_loc.get_current_line ());
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

let rec build_ast_node_debug t =
    H2o_syntax.incr_depth ();
    let open H2o_par in
    let s = match t with
    | `Node (n, attrs, c) ->
            sprintf "`Node(%s, %s, [\n%s%s])\n"
                n
                (H2o_list.enum ~sep:"; " attrs
                    (fun (k, v) -> sprintf "%s=%s" k v))
                (H2o_list.enum ~sep:"\n" c (fun c -> (H2o_syntax.make_pad ()) ^ (build_ast_node_debug c)))
                (H2o_syntax.make_pad ())
    | `Data d -> sprintf "`Data(%s)\n" d
    | `Comment c -> sprintf "`Comment(%s)\n" c
    | `Eof -> sprintf "`Eof\n"
    in
    H2o_syntax.decr_depth ();
    s

let print_ast_node_debug t =
    printf "%s\n" (build_ast_node_debug t)
