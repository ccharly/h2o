{
  open Printf
  open H2o_par
  open H2o_loc

  let tag_end = ref false

  module Map = Map.Make(struct
      type t = string
      let compare = compare
  end)

  (* Elements that does not required any children. They can be written like:
   * <img ...> (without </img> or .. />)
   * *)
  let nullary =
      let m = Map.empty in
      List.fold_left (fun m n -> Map.add n true m) m [
          (* Add nullary element here *)
          "img";
      ]

  let is_nullary n =
      Map.mem n nullary
}

let spaces = (' ' | '\t')
let blank = (spaces | '\n')

let num = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let tname = alpha (alpha | num | '-' | '_')*
let aname = alpha (alpha | num | '-' | '-' | ':')*

rule html = parse
  | '\r' { html lexbuf }
  | '\n'
    { !+ 1;
      html lexbuf }
  | spaces+
    { html lexbuf }
  | ([^ '<' '>' '\n']+ as data)
    { Data data }
  | "<" (blank* as b) (tname as n)
    { !+ (count_eol b);
      let a = tag_inner lexbuf in
      if not !tag_end
      then begin
          if is_nullary n
          then Tag (n, a)
          else TagStart (n, a)
      end else begin
          tag_end := false;
          Tag (n, a)
      end }
  | "</" (blank* as b) (tname as n) (blank* as bb) ">"
    { !+ (count_eol b);
      !+ (count_eol bb);
      if is_nullary n
      then Comment ("discarding unexpected </"^n^">")
      else TagEnd n }
  | "<![CDATA[" (_* as d) "]]>"
    { !+ (count_eol d);
      CDATA d }
  | "<!DOCTYPE" ([^ '>']* as d) ">"
    { DocType d }
  | "<!--" (('-'* [^ '-' '>'] | '>')* as c) "-->"
    { Comment c }
  | eof
    { EOF }
and tag_inner = parse
  | '\r' { tag_inner lexbuf }
  | '\n'
    { !+ 1;
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