OCAMLC=ocamlc

all: warmup hanoi

warmup: warmup.ml
	$(OCAMLC) -o warmup util.ml warmup.ml

hanoi: hanoi.ml
	$(OCAMLC) -o hanoi util.ml hanoi.ml


clean:
	rm -f *.cmi *.cmo warmup hanoi
