%{
    open Printf

    let name_of_tag (name, _) = name
    let attrs_of_tag (_, attrs) = attrs

    let node_of_tag (name, attrs) l =
        `Node (name, attrs, (List.rev l))
%}

%token EOF
%token <string> DocType
%token <string> CDATA
%token <string * (string * string) list> Tag
%token <string * (string * string) list> TagStart
%token <string * (string * string) list * string> TagRaw
%token <string> TagEnd
%token <string> Comment
%token <string> Data

%start root
%type <H2o_ast.t> root

%%

root:
  | EOF { `Eof }
  | doc { $1 }
;
doc:
  | Tag { node_of_tag $1 [] }
  | TagRaw {
      let name, attrs, data = $1 in
      `Node (name, attrs, [`Data data])
  }
  | TagStart doc_list TagEnd {
    let name = name_of_tag $1 in
    if name = $3 then begin
        node_of_tag $1 (List.rev $2)
    end else
        failwith (sprintf "parsing:%d: unexpected %s, was expecting %s" (H2o_loc.get_current_line ()) $3 name)
  }
  | CDATA { `Comment $1 }
  | Comment { `Comment $1 }
  | DocType { `Comment $1 }
;
doc_list:
  | Data doc_list { (`Data $1)::$2 }
  | doc doc_list  { $1::$2 }
  | { [] }
;
