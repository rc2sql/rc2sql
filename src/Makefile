all: rtrans.native vgtrans.native

rtrans.native: rtrans.ml trans.ml FO.ml Misc.ml db_lexer.mll db_parser.mly fo_lexer.mll fo_parser.mly verified.ml
	ocamlbuild -use-menhir -package zarith rtrans.native

vgtrans.native: vgtrans.ml trans.ml FO.ml Misc.ml db_lexer.mll db_parser.mly fo_lexer.mll fo_parser.mly verified.ml
	ocamlbuild -use-menhir -package zarith vgtrans.native
