OCAMLMAKEFILE = OCamlMakefile

SOURCES       = uni_data.ml uni_norm.ml stringprep.ml \
		auth.ml xmpp.ml error.ml jeps.ml
PRE_TARGETS   = xmlstream.cmx xmlstream.cmo
INCDIRS       = ../libs/xml

OCAMLNLDFLAGS   = xmlstream.cmx
OCAMLBLDFLAGS   = xmlstream.cmo

RESULT         = xmpp

all: ncl bcl

xmlstream.cmo: xmlstream.ml
	ocamlfind ocamlc  -syntax camlp4o -I ../libs/xml -package ulex -linkpkg -c xmlstream.ml
xmlstream.cmx: xmlstream.ml
	ocamlfind ocamlopt -syntax camlp4o -I ../libs/xml -package ulex -linkpkg -c xmlstream.ml

include $(OCAMLMAKEFILE)

