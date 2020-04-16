MENHIR=menhir
OCAMLLEX=ocamllex

flambda_lex.ml: flambda_lex.mll
	${OCAMLLEX} flambda_lex.mll

flambda_parser-in.ml flambda_parser-in.mli: flambda_parser.mly
	${MENHIR} --table flambda_parser.mly --base flambda_parser-in

flambda_parser.ml: flambda_parser-in.ml
	sed "s/MenhirLib/CamlinternalMenhirLib/g" $< > $@

flambda_parser.mli: flambda_parser-in.mli
	sed "s/MenhirLib/CamlinternalMenhirLib/g" $< > $@

flambda_parser_messages.ml: flambda_parser.mly flambda_parser.messages
	${MENHIR} --compile-errors flambda_parser.messages flambda_parser.mly > flambda_parser_messages.ml

flambda_parser.new-messages: flambda_parser.mly
	${MENHIR} --list-errors flambda_parser.mly > flambda_parser.new-messages

.PHONY: check-errors

check-errors: flambda_parser.messages flambda_parser.new-messages
	${MENHIR} --compare-errors flambda_parser.new-messages --compare-errors flambda_parser.messages flambda_parser.mly
