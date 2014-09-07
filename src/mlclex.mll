{
  open Printf
  open Mlcpar

  let tag_end = ref false

  let count_eol s =
      let i = ref 0 in
      String.iter (fun c -> if c = '\n' then incr i) s;
      !i

  let current_line = ref 1

  let (!+) n =
      current_line := !current_line + n

  let get_current_line () =
      !current_line
}

let spaces = (' ' | '\t')
let blank = (spaces | '\n')

let num = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let tname = alpha (alpha | num | '-' | '_')*
let aname = alpha (alpha | num | '-' | '-')*

rule html = parse
  | '\n'
    { incr current_line;
      html lexbuf }
  | spaces+
    { html lexbuf }
  | ([^ '<' '>' '\n']+ as data)
    { Data data }
  | "<" (blank* as b) (tname as n)
    { !+ (count_eol b);
      let a = tag_inner lexbuf in
      if not !tag_end
      then TagStart (n, a)
      else begin
          tag_end := false;
          Tag (n, a)
      end }
  | "</" (blank* as b) (tname as n) (blank* as bb) ">"
    { !+ (count_eol b);
      !+ (count_eol bb);
      TagEnd n }
  | "<!DOCTYPE" ([^ '>']* as d) ">"
    { DocType d }
  | "<!--" (([^ '-'] | "- ")* as c) "-->"
    { Comment c }
  | eof
    { printf "lines:%d\n" !current_line; EOF }
and tag_inner = parse
  | '\n'
    { incr current_line;
      tag_inner lexbuf }
  | spaces+
    { tag_inner lexbuf }
  | (aname as n) '='  '"' ([^ '"']*  as v) '"'
    { (n, v)::(tag_inner lexbuf) }
  | (aname as n) '=' '\'' ([^ '\'']*  as v) '\''
    { (n, v)::(tag_inner lexbuf) }
  | (aname as n) '=' ([^ '\'' ' ' '\t' '\n' '"' '/' '>']*  as v)
    { (n, v)::(tag_inner lexbuf) }
  | (aname as n)
    { (n, "")::(tag_inner lexbuf) }
  | "/>"
    { tag_end := true; [] }
  | ">"
    { [] }
