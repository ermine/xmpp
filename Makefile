OCAMLMAKEFILE = ../OCamlMakefile

SOURCES         = auth.ml xmpp.ml jeps.ml
THREADS         = yes
PACKS           = ulex
OCAMLFLAGS      = nums.cmxa cryptokit.cmxa -I ../xml xml.cmxa
OCAMLLDFLAGS    = xmlstream.cmx -linkall -linkpkg -pack
 
RESULT          = xmpp

all: ncl

xmlstream.cmx: xmlstream.ml
	ocamlfind opt -I ../xml -package ulex,camlp4 -syntax camlp4o -c xmlstream.ml


include $(OCAMLMAKEFILE)

