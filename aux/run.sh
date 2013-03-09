#! /bin/sh

set  -E

rm -f xmpp_prep_tables.ml

ocamlbuild xmpp_prep_tables_generator.byte
ocamlbuild ucd_reader.byte

DATE=`date "+%Y-%m-%d"`

cat > xmpp_prep_tables.ml << EOF
(*
 * (c) 2013 Anastasia Gornostaeva 
 * Generated at ${DATE}
 *)

EOF

./xmpp_prep_tables_generator.byte >> xmpp_prep_tables.ml
./ucd_reader.byte unicode-3.2/UnicodeData-3.2.0.txt unicode-3.2/CompositionExclusions-3.2.0.txt >> xmpp_prep_tables.ml 

