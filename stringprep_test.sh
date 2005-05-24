#! /bin/sh
ocamlfind ocamlopt -package ulex,str -linkpkg \
	uni_data.ml uni_norm.cmx stringprep.ml \
	stringprep_test.ml -o test
