OCAMLMAKEFILE = ../OCamlMakefile

SOURCES := xmlstream.ml auth.ml xmpp.ml jeps.ml
THREADS := yes
PACKS := ulex
OCAMLLDFLAGS := -linkall -linkpkg -pack
OCAMLFLAGS := nums.cmxa cryptokit.cmxa -I ../xml xml.cmxa
#OCAMLLDFLAGS := nums.cmxa cryptokit.cmxa ../xml/xml.cmxa
 
RESULT := xmpp

all: native-code-library
#all: native-code


include $(OCAMLMAKEFILE)

