{
  open Printf
  open H2o_par
  open H2o_loc

  let tag_end = ref false

  module Map = Map.Make(struct
      type t = string
      let compare = compare
  end)

  (* [true] if you want to enable the replace of escape HTML to
   * eliom string *)
  let enable_replace = true

  (* Replace some escaped HTML string to their original form
   * FIXME: Try to make it faster *)
  let to_replace = [
      "&quot;", "\"";
      "&lt;", "<";
      "&gt;", ">";
      "&amp;", "&";
  ]

  let to_replace =
      List.map (fun (r, c) -> Str.regexp r, c) to_replace

  let to_eliom_string s =
      if not enable_replace then s
      else
          H2o_list.fold_left s to_replace
            (fun s (r, c) -> Str.global_replace r c s)

  (* Elements that does not required any children. They can be written like:
   * <img ...> (without </img> or .. />)
   * *)
  let nullary =
      let m = Map.empty in
      List.fold_left (fun m n -> Map.add n true m) m [
          (* Add nullary element here *)
          "img";
          "meta";
          "link";
          "hr";
          "br";
          "input";
      ]

  (* Elements that can contain raw data such as html elements
   * without any parsing check, like:
   * <script> <p> p not closed </script>
   *
   * *)
  let raw_content =
      let m = Map.empty in
      List.fold_left (fun m n -> Map.add n true m) m [
          (* Add nullary element here *)
          "script";
      ]

  let is_nullary n =
      Map.mem n nullary

  let has_raw_content n =
      Map.mem n raw_content
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
    { Data (to_eliom_string data) }
  | "<" (blank* as b) (tname as n)
    { !+ (count_eol b);
      let a = tag_inner lexbuf in
      if not !tag_end
      then begin
          if has_raw_content n
          then TagRaw (n, a, tag_raw n lexbuf)
          else begin
            if is_nullary n
            then Tag (n, a)
            else TagStart (n, a)
          end
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
    { !+ (count_eol c);
      Comment c }
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
and tag_raw name = parse
  | '\r' { tag_raw name lexbuf }
  | '\n'
    { !+ 1;
      "\n" ^ tag_raw name lexbuf }
  | "</" (blank* as b) (tname as n) (blank* as bb) ">"
    { !+ (count_eol b);
      !+ (count_eol bb);
      if n = name
      then ""
      else tag_raw name lexbuf }
  | ([^ '<' '\n']+ as data)
    { (to_eliom_string data) ^ (tag_raw name lexbuf) }
  | '<'
    { "<" ^ (tag_raw name lexbuf) }
