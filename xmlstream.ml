(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml

let regexp space = ' ' | '\n' | '\t' | '\r'

let regexp any = xml_letter | xml_digit | xml_extender | xml_base_char 
               | xml_ideographic | xml_combining_char | xml_blank

let regexp name_char = xml_letter | xml_digit | '.' | '-' | '_' | ':'
                     | xml_combining_char | xml_extender
let regexp name = (xml_letter | '_' | ':') (name_char* )

let regexp ent = "&lt;" | "&gt;" | "&amp;" | "&quot;" | "&apos;"

exception XmlError of string * string

let enclosed lexbuf offset_begin offset_end =
   let len = Ulexing.lexeme_length lexbuf in
      Ulexing.utf8_sub_lexeme lexbuf offset_begin
         (len - (offset_end + offset_begin))

let parse_error buf str =
   raise (XmlError (buf, str))

type t =
   | StreamError of element list
   | StreamEnd
   | Element of element

let rec stream decode = lexer
   | eof -> 
	StreamEnd
   | space+ ->
	stream decode lexbuf
   | "<?xml" ->
	Element (xmldecl [] lexbuf)
   | "<stream:stream" ->
	let empty, attrs = attributes decode [] lexbuf in
	   if empty || attrs = [] then
	      parse_error  "No attrs or features" "Invalud XML"
	   else
	      Element 
		 (Xmlelement ("stream:stream", attrs, []))
(*
			      [stream_features lexbuf]))
*)
   | "<stream:features" space* '>' ->
	Element 
	   (Xmlelement ("stream:features", [], 
			elements decode "stream:features" [] lexbuf))

   | "<stream:error>" ->
	StreamError (elements decode "stream:error" [] lexbuf)
   | "</stream:stream>" ->
	StreamEnd
   | '<' ->
	let tag = name lexbuf in
	let empty, attrs = attributes decode [] lexbuf in
	let subels = match empty with
	   | true -> []
	   | false -> elements decode tag [] lexbuf
	in
	let el = Xmlelement (tag, attrs, subels) in
	   Element el
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [stream]"
(*
and stream_features = lexer 
   | space* "<stream:features" space* '>' ->
	Xmlelement ("stream:features", [], elements "stream:features" [] lexbuf)
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) 
	    "Invalid XMPP [stream_features]"
*)
and element decode = lexer
   | name ->
	let tag = Ulexing.utf8_lexeme lexbuf in
	let empty, attrs = attributes decode [] lexbuf in
	   if empty then
	      Xmlelement (tag, attrs, [])
	   else
	      Xmlelement (tag, attrs, (elements decode tag [] lexbuf))
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [ekenebt]"

and elements decode tag acc = lexer
   | "</" ->
	let endtag = name lexbuf in
	   if endtag = tag then 
	      begin
		 eat_end lexbuf;
		 List.rev acc
	      end
	   else
	      parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [elements]"
   | '<' ->
	elements decode tag ((element decode lexbuf) :: acc) lexbuf
   | [^'<'] ->
	Ulexing.rollback lexbuf;
	let cdatatxt =
	   if decode then
	      cdata_with_decode "" lexbuf
	   else
              cdata lexbuf 
	in
	   elements decode tag ((Xmlcdata cdatatxt) :: acc) lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [elements]"

and xmldecl attrs = lexer
   | space+ ->
	let attr = attribute false lexbuf in
	   xmldecl (attr :: attrs) lexbuf
   | space* "?>" space* "<stream:stream" ->
	if (try List.assoc "version" attrs = "1.0" with _ -> false)
	then
	   if (try List.assoc "encoding" attrs = "UTF-8" with _ -> true)
	   then 
	      let empty, attrs = attributes false [] lexbuf in
		 if empty || attrs = [] then
		    parse_error (Ulexing.utf8_lexeme lexbuf) "Invalud XML"
		 else
		    Xmlelement ("stream:stream", attrs, [])
				   (* [stream_features lexbuf]) *)
	   else parse_error (Ulexing.utf8_lexeme lexbuf) 
	      (Printf.sprintf "Invalid encoding %s"
				       (List.assoc "encoding" attrs))
	else parse_error (Ulexing.utf8_lexeme lexbuf) 
	   (Printf.sprintf "Invalid version: %s" 
	       (List.assoc "version" attrs))


   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [xmldecl"

and name = lexer
   | name -> 
	Ulexing.utf8_lexeme lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid char in tag"

and attributes decode attrs = lexer
   | space* "/>" -> 
	true, attrs
   | space* '>' -> 
	false, attrs
   | space+ ->
	let attr = attribute decode lexbuf in
           attributes decode (attr :: attrs) lexbuf
   | _ ->
	parse_error (Ulexing.utf8_lexeme lexbuf) "No space here"

and attribute decode = lexer
   | name '=' ->
	let attrname = enclosed lexbuf 0 1 in
           attrname, attvalue decode lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid attribute name"

and attvalue decode = lexer
   | "\"" -> 
	attvalue_quot decode "" lexbuf
   | "'" -> 
	attvalue_apos decode "" lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "No quotes"

and attvalue_quot decode value = lexer
   | [^ "<&\""]* ->
	attvalue_quot decode (value ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf
   | ent ->
	if decode then
	   let s = match Ulexing.utf8_lexeme lexbuf with
	      | "&lt;" -> "<"
	      | "&gt;" -> ">"
	      | "&amp;" -> "&"
	      | "&apos;" -> "'"
	      | "&quot;" -> "\""
	      | other -> other
	   in attvalue_quot decode (value ^ s) lexbuf
	else
	   attvalue_quot decode (value ^ Ulexing.utf8_lexeme lexbuf) lexbuf
   | "\"" -> 
	value
   | _ -> 
print_endline "not expected here";
print_endline value;
	parse_error (Ulexing.utf8_lexeme lexbuf) 
	   "Not expected in attribute value"

and attvalue_apos decode value = lexer
   | [^"<&'"]+ ->
	attvalue_apos decode (value ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf
   | ent ->
	if decode then
	   let s = match Ulexing.utf8_lexeme lexbuf with
	      | "&lt;" -> "<"
	      | "&gt;" -> ">"
	      | "&amp;" -> "&"
	      | "&apos;" -> "'"
	      | "&quot;" -> "\""
	      | other -> other
	   in attvalue_apos decode (value ^ s) lexbuf
	else
	   attvalue_apos decode (value ^ Ulexing.utf8_lexeme lexbuf) lexbuf
   | "'" -> 
	value
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) 
	   "Not expected in attrvalue value"

and cdata = lexer
   | ([^"<&"] | ent)* -> 
	Ulexing.utf8_lexeme lexbuf
   | any ->
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [cdata]"

and cdata_with_decode acc = lexer
   | [^"<&"]+ ->
	cdata_with_decode (acc ^ Ulexing.utf8_lexeme lexbuf) lexbuf
   | ent ->
	let s = match Ulexing.utf8_lexeme lexbuf with
	   | "&lt;" -> "<"
	   | "&gt;" -> ">"
	   | "&amp;" -> "&"
	   | "&apos;" -> "'"
	   | "&quot;" -> "\""
	   | other -> other
	in cdata_with_decode (acc ^ s) lexbuf
   | "<" ->
	Ulexing.rollback lexbuf;
	acc
   | any ->
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [cdata]"

and eat_end = lexer
   | space* '>' ->
	()
   | any ->
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [eat_end]"

let from_stream ?(decode=false) s =
   let lexbuf = Ulexing.from_utf8_stream s in
   let z () =
      stream decode lexbuf
   in z

let parse_stream ?(decode=false) channel =
   let lexbuf = Ulexing.from_utf8_channel channel in
   let z () =
      stream decode lexbuf
   in z
