OCAMLBUILD  = ocamlbuild
OPTIONS		= -classic-display -use-ocamlfind

PACKAGES	= cppo_ocamlbuild
PACKAGES	:= -plugin-tag $(patsubst %,"package(%)",$(PACKAGES))

OCAMLBUILD  := $(OCAMLBUILD) $(OPTIONS) $(PACKAGES)

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
