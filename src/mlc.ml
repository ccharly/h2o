open Mlcpar
open Printf

let print_token t = match t with
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

let () =
    let file = "test/big.html" in
    let in_ch = open_in file in
    let lexbuf = Lexing.from_channel in_ch in
    let eof = ref false in
    while not !eof do
        let token = Mlclex.html lexbuf in
        print_token token;
        if token = EOF then
            eof := true
    done;
