(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Unix
open Xml
open XMPP

let os = (let f = open_process_in "uname -sr" in
          let answer = input_line f in
            ignore (close_process_in f); answer)

let ns_version = Some "jabber:iq:version"

let iq_version_reply name version xml =
  make_iq_reply ~payload:
    [make_element (ns_version, "query") []
       [make_simple_cdata (ns_version, "name") name;
		    make_simple_cdata (ns_version, "version")
			    (Printf.sprintf "%s (Ocaml %s)" 
			       version Sys.ocaml_version);
		    make_simple_cdata (ns_version, "os") os]
		] xml

(* JEP 54 vCard-temp *)

let ns_vcard = Some "vcard-temp"

let iq_vcard_query ?jid_from ?lang ~id jid_to =
  make_iq ~id ?jid_from ~jid_to ~type_:`Get ?lang
    ~payload:[make_element (ns_vcard, "vcard") [] []] ()
    
let ns_last = Some "jabber:iq:last"

let iq_last_reply start_time xml =
  make_iq_reply ~type_:`Result
    ~payload:[make_element (ns_last, "query")
                [make_attr "seconds"
                   (string_of_int (int_of_float
                                     ((Unix.gettimeofday ()) -. start_time)))]
                []] xml

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
