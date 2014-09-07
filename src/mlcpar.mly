%{
%}

%token EOF
%token <string> DocType
%token <string * (string * string) list> Tag
%token <string * (string * string) list> TagStart
%token <string> TagEnd
%token <string> Comment
%token <string> Data

%start doc
%type <unit> doc

%%

doc:
  | EOF                   { () }
;
