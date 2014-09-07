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

let rec print_ast_node = function
    | `Node (n, a, c) ->
            printf "`Node:%s\n" n;
            List.iter (fun (a,b) -> printf "attr:%s=%s\n" a b) a;
            printf "[\n";
            List.iter print_ast_node c;
            printf "]\n"
    | `Data d ->
            printf "`Data:%s\n" d
    | `Comment c ->
            printf "`Comment:%s\n" c
    | `Eof ->
            printf "`Eof\n"
