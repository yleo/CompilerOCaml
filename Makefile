

compilo: expr.cmo expr_int.cmo lexer.cmo parser.cmo symbol_type.cmo analyse.cmo symbol_table.cmo prompt_lexer.cmo prompt_parser.cmo expr_int.cmo geninter.cmo liveness.cmo gencode.cmo main.cmo 
	ocamlc -g -I ocamlgraph/ graph.cma expr.cmo expr_int.cmo lexer.cmo parser.cmo symbol_type.cmo analyse.cmo symbol_table.cmo prompt_parser.cmo prompt_lexer.cmo expr_int.cmo geninter.cmo liveness.cmo gencode.cmo main.cmo -o compilo

symbol_type.cmo: symbol_type.ml expr.cmo expr_int.cmo
	ocamlc -g -c symbol_type.ml

symbol_table.cmo: symbol_table.ml analyse.cmo expr.cmo symbol_type.cmo
	ocamlc -g -c symbol_table.ml

analyse.cmo: analyse.ml expr.cmo  symbol_type.cmo
	ocamlc -g -c analyse.ml

expr.cmo: expr.ml
	ocamlc -g -c expr.ml

expr_int.cmo : expr_int.ml 
	ocamlc -g -c expr_int.ml

parser.cmo: expr.cmo parser.ml lexer.cmo
	ocamlc -g -c parser.ml

parser.mli: expr.cmo parser.mly
	ocamlyacc parser.mly

parser.ml: expr.cmo parser.mli
	ocamlc -g -c parser.mli

lexer.cmo: expr.cmo lexer.ml parser.ml
	ocamlc -g -c lexer.ml

lexer.ml: expr.cmo lexer.mll
	ocamllex lexer.mll

main.cmo: main.ml lexer.cmo parser.cmo expr.cmo symbol_table.cmo prompt_parser.cmo prompt_lexer.cmo
	ocamlc -g -c main.ml

clean:
	rm -f *.cmo *.cmi *.mli compilo parser.ml lexer.ml

geninter.cmo: geninter.ml expr_int.cmo symbol_table.cmo symbol_type.cmo analyse.cmo
	ocamlc -g -c geninter.ml	

gencode.cmo: gencode.ml expr_int.cmo symbol_table.cmo symbol_type.cmo analyse.cmo liveness.cmo
	ocamlc -g -c gencode.ml	

liveness.cmo: expr_int.cmo liveness.ml
	ocamlc -g -c -I ocamlgraph/ liveness.ml

prompt_parser.cmo: prompt_parser.ml prompt_lexer.cmo
	ocamlc -g -c prompt_parser.ml

prompt_parser.mli: prompt_parser.mly
	ocamlyacc prompt_parser.mly

prompt_parser.ml: prompt_parser.mli
	ocamlc -g -c prompt_parser.mli

prompt_lexer.cmo: prompt_lexer.ml prompt_parser.ml
	ocamlc -g -c prompt_lexer.ml

prompt_lexer.ml: prompt_lexer.mll
	ocamllex prompt_lexer.mll

