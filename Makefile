OCAMLMAKEFILE = ../OCamlMakefile

SOURCES       = uni_data.ml uni_norm.ml stringprep.ml \
		auth.ml xmpp.ml error.ml jeps.ml
PRE_TARGETS   = xmlstream.cmx
#THREADS       = yes
#USE_CAMLP4    = yes
PACKS         = ulex
INCDIRS       = ../libs/xml
OCAMLLDFLAGS   = xmlstream.cmx
#OCAMLLDFLAGS  = nums.cmxa cryptokit.cmxa xml.cmxa \
#OCAMLDEP      = ocamldep -package ulex -syntax camlp4o 
RESULT         = xmpp

all: ncl

xmlstream.cmx: xmlstream.ml
	ocamlfind ocamlopt -syntax camlp4o -I ../libs/xml -package ulex -linkpkg -c xmlstream.ml

include $(OCAMLMAKEFILE)

