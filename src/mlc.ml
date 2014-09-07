open Printf

let () =
    let open Mlcpar in
    let file = "test/big.html" in
    let in_ch = open_in file in
    let lexbuf = Lexing.from_channel in_ch in
    let eof = ref false in
    while not !eof do
        let token = Mlclex.html lexbuf in
        Mlcprinter.print_token token;
        if token = EOF then
            eof := true
    done
