OCAMLBUILD  = ocamlbuild
OPTIONS		= -classic-display -use-ocamlfind

PLUGINS		= cppo_ocamlbuild
PLUGINS		:= -plugin-tag $(patsubst %,"package(%)",$(PLUGINS))

PKGS		= str
PKGS		:= $(addprefix -package ,$(PKGS))

OCAMLBUILD  := $(OCAMLBUILD) $(OPTIONS) $(PLUGINS) $(PKGS)

PARSER		= mlcpar
LEXER		= mlclex

BIN			= mlc

all: lexer parser
	$(OCAMLBUILD) src/$(BIN).native src/$(BIN).byte

lexer:
	$(OCAMLBUILD) src/$(LEXER).ml

parser:
	$(OCAMLBUILD) src/$(PARSER).ml

clean:
	$(OCAMLBUILD) -clean

.PHONY:
	clean lexer parser
