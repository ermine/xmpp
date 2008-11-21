OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES		= uni_data.ml uni_norm.ml stringprep.ml \
		  jid.mli jid.ml sasl.ml xmpp.ml error.ml \
		  jeps.ml xdata.ml disco.ml xcommands.ml
PACKS		= cryptokit xml
PRE_TARGETS	= xmlstream.cmx xmlstream.cmo
OCAMLNLDFLAGS	= xmlstream.cmx
OCAMLBLDFLAGS	= xmlstream.cmo

RESULT		= xmpp
TRASH		= xmlstream.cm* xmlstream.o

include ../../Makefile.global
include ../Makefile.inc
LIBINSTALL_FILES += jid.cmi jid.mli error.cmi jeps.cmi xcommands.cmi sasl.cmi stringprep.cmi xdata.cmi disco.cmi

all: ncl bcl

xmlstream.cmo: xmlstream.ml
	ocamlfind ocamlc  -syntax camlp4o -package ulex,xml -c xmlstream.ml
xmlstream.cmx: xmlstream.ml
	ocamlfind ocamlopt -syntax camlp4o -package ulex,xml -c xmlstream.ml

include $(OCAMLMAKEFILE)

