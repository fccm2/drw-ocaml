all: lst.cmo
lst.cmi: lst.mli
	ocamlc -c $<
lst.cmo: lst.ml lst.cmi
	ocamlc -c $<
test: lst.cmo
	ocaml -I ../mini-svg-0.03.9d/src/ drw.ml
clean:
	$(RM) lst.cmi
	$(RM) lst.cmo
