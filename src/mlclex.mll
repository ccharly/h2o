{
  open Printf
  open Mlcpar

  let tag_end = ref false

  let current_line = ref 1

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
  | ([^ '<' '>']+ as data)
    { Data data }
  | "<" blank* (tname as n)
    { let a = tag_inner lexbuf in
      if not !tag_end
      then TagStart (n, a)
      else begin
          tag_end := false;
          Tag (n, a)
      end }
  | "</" blank* (tname as n) blank* ">"
    { TagEnd n }
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
