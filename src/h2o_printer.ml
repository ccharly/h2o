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

let rec print_ast_node node =
    printf "%s\n" (H2o_node_printer.build node)
