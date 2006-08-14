(*
 * (c) 2004, 2005, 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Unix
open Xmpp
open Xml

let os = (let f = open_process_in "uname -sr" in
          let answer = input_line f in
             ignore (close_process_in f); answer)

let iq_version_reply name version xml =
   iq_reply ~subels:[make_simple_cdata "name" name;
		     make_simple_cdata "version"
			(Printf.sprintf "%s (Ocaml %s)" 
			    version Sys.ocaml_version);
		     make_simple_cdata "os" os
		    ] xml

(* JEP 54 vCard-temp *)

let iq_vcard_query ?from ?lang ~id to_ =
   make_iq ~id ?from ~to_ ~type_:`Get ?lang 
      ~xmlns:"vcard-temp" ~query_tag:"vCard" ()
      
let iq_last_reply start_time xml =
   match xml with
      | Xmlelement (_, attrs, _) ->
	   let newattrs = make_attrs_reply ~type_:"result" attrs in
	      Xmlelement ("iq", newattrs, 
			  [Xmlelement ("query", ["xmlns", "jabber:iq:last";
						 "seconds", 
	string_of_int (int_of_float ((Unix.gettimeofday ()) -. start_time))],
				       [])])
      | _ -> raise NonXmlelement

(* jep 90 *)
(*
let iq_time_reply xml =
   let time = Unix.gettimeofday () in
      iq_reply xml
      ~subels:[make_simple_cdata "utc" "";
	       (* make_simple_cdata "tz" ""; *)
	       make_simple_cdata "display" ""]

*)
(*
     <utc>20020910T17:58:35</utc>                                               
     <tz>MDT</tz>                                                               
     <display>Tue Sep 10 12:58:35 2002</display>                                
*)
