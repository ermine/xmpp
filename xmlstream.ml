(*pp camlp4o pa_o.cmo pa_op.cmo pr_dump.cmo -I /usr/local/lib/ocaml/site-lib/ulex pa_ulex.cma *)

open Xml

let regexp space = ' ' | '\n' | '\t' | '\r'

let regexp any = xml_letter | xml_digit | xml_extender | xml_base_char 
               | xml_ideographic | xml_combining_char | xml_blank

let regexp name_char = xml_letter | xml_digit | '.' | '-' | '_' | ':'
                     | xml_combining_char | xml_extender
let regexp name = (xml_letter | '_' | ':') (name_char*)

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

let rec stream = lexer
   | eof -> 
	StreamEnd
   | space+ ->
	stream lexbuf
   | "<?xml" ->
	Element (xmldecl [] lexbuf)
   | "<stream:stream" ->
	let empty, attrs = attributes [] lexbuf in
	   if empty || attrs = [] then
	      parse_error  "No attrs or features" "Invalud XML"
	   else
	      Element 
		 (Xmlelement ("stream:stream", attrs, 
			      [stream_features lexbuf]))

   | "<stream:error>" ->
	StreamError (elements "stream:error" [] lexbuf)
   | "</stream:stream>" ->
	StreamEnd
   | '<' ->
	let tag = name lexbuf in
	let empty, attrs = attributes [] lexbuf in
	let subels = match empty with
	   | true -> []
	   | false -> elements tag [] lexbuf
	in
	let el = Xmlelement (tag, attrs, subels) in
	   Element el
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [stream]"

and stream_features = lexer 
   | space* "<stream:features" space* '>' ->
	Xmlelement ("stream:features", [], elements "stream:features" [] lexbuf)
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) 
	    "Invalid XMPP [stream_features]"

and element = lexer
   | name ->
	let tag = Ulexing.utf8_lexeme lexbuf in
	let empty, attrs = attributes [] lexbuf in
	   if empty then
	      Xmlelement (tag, attrs, [])
	   else
	      Xmlelement (tag, attrs, (elements tag [] lexbuf))
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [ekenebt]"

and elements tag acc = lexer
   | "</" ->
	let endtag = name lexbuf in
	   if endtag = tag then 
	      begin
		 eat_end lexbuf;
		 acc
	      end
	   else
	      parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [elements]"
   | '<' ->
	elements tag ((element lexbuf) :: acc) lexbuf
   | [^'<'] ->
	let cdatatxt = Ulexing.utf8_lexeme lexbuf in
        let cdatatxt = cdatatxt ^ cdata lexbuf in
	   elements tag ((Xmlcdata cdatatxt) :: acc) lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [elements]"

and xmldecl attrs = lexer
   | space+ ->
	let attr = attribute lexbuf in
	   xmldecl (attr :: attrs) lexbuf
   | space* "?>" space* "<stream:stream" ->
	if (try List.assoc "version" attrs = "1.0" with _ -> false)
	then
	   if (try List.assoc "encoding" attrs == "UTF-8" with _ -> true)
	   then 
	      let empty, attrs = attributes [] lexbuf in
		 if empty || attrs == [] then
		    parse_error (Ulexing.utf8_lexeme lexbuf) "Invalud XML"
		 else
		    Xmlelement ("stream:stream", attrs, 
				[stream_features lexbuf])
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

and attributes attrs = lexer
   | space? "/>" -> 
	true, attrs
   | space? '>' -> 
	false, attrs
   | space+ ->
	let attr = attribute lexbuf in
           attributes (attr :: attrs) lexbuf
   | _ ->
	parse_error (Ulexing.utf8_lexeme lexbuf) "No space here"

and attribute = lexer
   | name '=' ->
	let attrname = enclosed lexbuf 0 1 in
           attrname, attvalue lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid attribute name"

and attvalue = lexer
   | "\"" -> 
	attvalue_quot "" lexbuf
   | "'" -> 
	attvalue_apos "" lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "No quotes"

and attvalue_quot value = lexer
   | ([^"<&\""] | ent)* ->
	attvalue_quot (value ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf
   | "\"" -> 
	value
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Not expected here"

and attvalue_apos value = lexer
   | ([^"<&'"] | ent)* ->
	attvalue_apos (value ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf
   | "'" -> 
	value
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Not expected here"

and cdata = lexer
   | ([^"'<&"] | ent)* -> 
	Ulexing.utf8_lexeme lexbuf
   | any ->
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [cdata]"

and eat_end = lexer
   | space* '>' ->
	()
   | any ->
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [eat_end]"


let parse_stream channel =
   let lexbuf = Ulexing.from_utf8_channel channel in
   let z () =
      stream lexbuf
   in z
