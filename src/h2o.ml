open Printf

let print_token = ref false

let usage () =
    eprintf "usage: %s <file1> <file2> ..\n" Sys.executable_name

let () =
    let argv = Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1) in
    let argc = Array.length argv in
    if argc < 1 then
        (usage (); exit 1);
    let argv =
        if argv.(0) = "-debug" then begin
            print_token := true;
            Array.sub argv 1 ((Array.length argv) - 1)
        end
        else argv
    in
    let open H2o_par in
    Array.iter
      (fun f ->
          printf "(* ## %s *)\n" f;
          printf "let page () =\n";
          let in_ch = open_in f in
          let lexbuf = Lexing.from_channel in_ch in
          let eof = ref false in
          while not !eof do
              if not !print_token then begin
                  let token = H2o_par.root H2o_lex.html lexbuf in
                  if token = `Eof then
                      eof := true;
                printf "%s\n" (H2o_node_printer.build token)
                (*
                H2o_printer.print_ast_node_debug token;
                *)
              end else begin
                  let token = H2o_lex.html lexbuf in
                  if token = EOF then
                      eof := true;
                H2o_printer.print_token token;
              end
          done;
          printf "(* ## (eof) %s *)\n" f;
        ) argv
