OCAMLC=ocamlc

all: nqueens expression hoftr


nqueens: nqueens.ml
	$(OCAMLC) -o nqueens util.ml nqueens.ml

expression: expression.ml
	$(OCAMLC) -o expression util.ml expression.ml

hoftr: hoftr.ml
	$(OCAMLC) -o hoftr util.ml hoftr.ml


clean:
	rm -f *.cmi *.cmo hoftr nqueens expression
