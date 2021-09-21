all: ail.native

ail.native: ail.ml db_lexer.mll db_parser.mly fo_lexer.mll fo_parser.mly verified.ml
	ocamlbuild -use-menhir -package zarith ail.native
