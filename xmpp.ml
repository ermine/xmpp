(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru                    *)
(*                                                                          *)

open Xmlstream
open Xml

exception XMPPError of string

let send_xml out (el:element) = out (Xml.element_to_string el)

let close_stream out =
   out "</stream:stream>"

let start_stream server =
   "<?xml version='1.0' ?><stream:stream version='1.0' to='" ^ server ^
   "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>"

let open_stream_client out lexer server username password resource =
   out (start_stream server);
   let stream () =
      match lexer () with
	 | Element el -> el
	 | StreamError els -> failwith "stream error"
	 | StreamEnd -> failwith "stream end"
   in
   let el =  stream () in
      match_tag "stream:stream" el;
      let mechanisms = Xml.get_tag el ["stream:features"; "mechanisms"] in
      let mels = Xml.get_subels ~tag:"mechanism" mechanisms in
      let m = List.map (function x -> Xml.get_cdata x) mels in
         Auth.sasl_auth m (send_xml out) server username password stream;
         out (start_stream server);
	 let el = stream () in
	    match_tag "stream:stream" el;
	    let bind = Xml.get_tag el ["stream:features"; "bind"] in
	       send_xml out 
		  (Xmlelement ("iq", ["type", "set";
				      "to", server; "id", "bind1"],
			       [Xmlelement ("bind",
				   ["xmlns","urn:ietf:params:xml:ns:xmpp-bind"],
				       [make_simple_cdata "resource" 
					   resource])]));
	       let el = stream () in
		  if match_xml el "iq" ["type", "result"; "id", "bind1"] then
		     let myjid = get_cdata ~path:["bind";"jid"] el in
			send_xml out
			   (Xmlelement ("iq", ["from", myjid; "type", "set";
					       "id", "session1"],
					[Xmlelement ("session", 
						     ["xmlns",
				         "urn:ietf:params:xml:ns:xmpp-session"],
						     [])]));
			let el = stream () in
			   if match_xml el "iq" ["type", "result"; 
						 "id","session1"] then
			      (myjid, send_xml out, stream)
			   else
			      raise (XMPPError "Session binding failed")
		  else
		     raise (XMPPError "Resource binding failed")

let connect server port logfile =
   let inet_addr =
      try Unix.inet_addr_of_string server with Failure("inet_addr_of_string") ->
         (Unix.gethostbyname server).Unix.h_addr_list.(0) in
   let sock_addr = Unix.ADDR_INET (inet_addr, port) in
   let in_stream, out_stream = Unix.open_connection sock_addr in
      if logfile <> "" then
         let f1, f2 = Unix.pipe () in
         let out_pipe = Unix.out_channel_of_descr f1 in
         let in_pipe = Unix.in_channel_of_descr f2 in
         let logfd = open_out_gen [Open_creat; Open_append] 0o666  logfile in
         let send_raw raw =
            Printf.fprintf logfd "OUT: %s\n" raw;
            flush logfd;
            output_string out_stream raw;
            flush out_stream
         in
         let rec debug_proxy () =
            (try
		let str = String.create 8192 in
		let size = input in_stream str 0 8192 in
		   if size = 0 then raise End_of_file;
		   let data = (String.sub str 0 size) in
                      Printf.fprintf logfd "IN: %s\n" data;
                      flush logfd;
                      output_string out_pipe data;
                      flush out_pipe;
             with exn -> close_out logfd; 
		raise exn);
            debug_proxy ()
         in
         let t = Thread.create debug_proxy () in
         let lexer = Xmlstream.parse_stream in_pipe in
            send_raw, lexer
      else
         let send_raw text =
            output_string out_stream text;
            flush out_stream
         in
         let lexer = Xmlstream.parse_stream in_stream in
            send_raw, lexer

let client username password resource ?(port=5222) ?(logfile="") server =
   let raw_out, lexer = connect server port logfile in
      open_stream_client raw_out lexer server username password resource

(*********)

let get_bare_jid jid =   
   let r = Str.regexp "\\(\\(.+@\\)?[^/]+\\)\\(/.+\\)?"; in
   let t = Str.string_match r jid 0 in
      (* Strings.tolower ( *) Str.matched_group 1 jid (* ) *)

let get_resource jid =
   let r = Str.regexp "/" in
      try
	 let pos = Str.search_forward r jid 0 in
	    Str.string_after jid (pos+1)
      with _ -> ""

let get_xmlns xml =
   let subel = List.find (function
                             | Xmlelement (_, _, _) -> true
                             | Xmlcdata _ -> false
                         ) (Xml.get_subels xml) in
      Xml.get_attr_s subel "xmlns"

let make_attrs_reply ?type_ attrs =
   let to_ = try List.assoc "to" attrs with Not_found -> ""
   and from = try List.assoc "from" attrs with Not_found -> "" in

   let a1 = List.remove_assoc "to" attrs in
   let a2 = List.remove_assoc "from" a1 in

   let a3 = Xml.filter_attrs (("from", to_) :: ("to", from) :: a2) in
      match type_ with
         | None -> a3
         | Some d ->
              let a4 = List.remove_assoc "type" a3 in
              let a5 = if d <> "" then ("type", d) :: a4 else a4 in
                 a5

let iq_reply ?(type_="result") xml newsubels =
   match xml with
      | Xmlelement (_, attrs, subels) ->
           begin
              let newattrs = make_attrs_reply attrs ~type_ in
              let q = List.find (function
                                    | Xmlelement (_, _, _) -> true
                                    | Xmlcdata _ -> false
                                ) subels in
                 match q with
                    | Xmlelement (qn, qa, _) ->
                         let a = try  [("xmlns", List.assoc "xmlns" qa)]
                         with _ -> qa in
                            Xmlelement ("iq", newattrs,
                                     [Xmlelement (qn, a, newsubels)])
                    | _ -> raise NonXmlelement
           end
      | _ -> raise NonXmlelement

let make_presence ?(attrs=[]) ?(from="") ?(id="") ?(type_="")
   ?(status="") ?(subels=[])  to_ =
   let a = if attrs <> [] then attrs else
      Xml.filter_attrs
         [("from", from);
          ("to", to_); ("id", id); ("type", type_)] in

   let s1 = if status <> "" then [make_simple_cdata "status" status] else [] in
   let s2 = if subels <> [] then subels @ s1 else s1 in
      Xmlelement ("presence", a, s2)
