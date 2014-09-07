open Printf

let print_token = true

let usage () =
    eprintf "usage: %s <file1> <file2> ..\n" Sys.executable_name

let () =
    let argv = Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1) in
    let argc = Array.length argv in
    if argc < 1 then
        (usage (); exit 1);
    let open Mlcpar in
    Array.iter
      (fun f ->
          printf "## %s\n" f;
          let in_ch = open_in f in
          let lexbuf = Lexing.from_channel in_ch in
          let eof = ref false in
          while not !eof do
              if not print_token then begin
                  let token = Mlcpar.doc Mlclex.html lexbuf in
                  if token = `Eof then
                      eof := true;
                Mlcprinter.print_ast_node token;
              end else begin
                  let token = Mlclex.html lexbuf in
                  if token = EOF then
                      eof := true;
                Mlcprinter.print_token token;
              end
          done;
          printf "## (eof) %s\n" f;
        ) argv
