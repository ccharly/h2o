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
%token <string> TagEnd
%token <string> Comment
%token <string> Data

%start doc
%type <Mlc_ast.t> doc

%%

doc:
  | EOF { `Eof }
  | Tag { node_of_tag $1 [] }
  | TagStart doc_list TagEnd {
    if (name_of_tag $1) = $3 then begin
        node_of_tag $1 (List.rev $2)
    end else
        failwith "unexpected .."
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
