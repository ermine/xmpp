(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru>                   *)
(*                                                                          *)

open Xmlstream
open Xml

exception XMPPError of string

let send_xml out (el:element) = out (Xml.element_to_string el)

let close_stream out =
   out "</stream:stream>"

type stream_t = Client | ComponentAccept | ComponentConnect

let start_stream ?streamtype server =
   "<?xml version='1.0' ?><stream:stream version='1.0' to='" ^ server ^
      "' xmlns='" ^ 
      (match streamtype with
	 | None -> "jabber:client"
	 | Some t -> match t with
	      | Client -> "jabber:client"
	      | ComponentAccept -> "jabber:component:accept"
	      | ComponentConnect -> "jabber:component:connect")
   ^ "' xmlns:stream='http://etherx.jabber.org/streams'>"

let open_stream_client out lexer server username password resource =
   out (start_stream server);
   let stream () =
      match lexer () with
	 | Element el -> el
	 | StreamError els -> failwith "stream error"
	 | StreamEnd -> failwith "stream end"
   in
   let el =  stream () in match_tag "stream:stream" el;
      let el = stream () in match_tag "stream:features" el;
	 let mechanisms = Xml.get_tag el ["mechanisms"] in
	 let mels = Xml.get_subels ~tag:"mechanism" mechanisms in
	 let m = List.map (function x -> Xml.get_cdata x) mels in
            Auth.sasl_auth m (send_xml out) server username password stream;
            out (start_stream server);
	    let el = stream () in match_tag "stream:stream" el;
	       let el = stream () in match_tag "stream:features" el;
		  let bind = Xml.get_tag el ["bind"] in
		     send_xml out 
			(Xmlelement ("iq", ["type", "set";
					    "to", server; "id", "bind1"],
				     [Xmlelement ("bind",
					["xmlns",
					 "urn:ietf:params:xml:ns:xmpp-bind"],
					[make_simple_cdata "resource" 
					    resource])]));
		     let el = stream () in
			if match_xml el "iq" 
			   ["type", "result"; "id", "bind1"] then
			      let myjid = get_cdata ~path:["bind";"jid"] el in
				 send_xml out
				    (Xmlelement ("iq", 
						 ["from", myjid; "type", "set";
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
				       raise 
					  (XMPPError "Session binding failed")
			else
			   raise (XMPPError "Resource binding failed")

let open_stream_service out lexer server name password =
   let stream () =
      match lexer () with
	 | Element el -> el
	 | StreamError els -> failwith "stream error"
	 | StreamEnd -> failwith "stream end"
   in
   out (start_stream ~streamtype:ComponentAccept server);

   let el =  stream () in
print_endline (Xml.element_to_string el);
flush Pervasives.stdout;
      match_tag "stream:stream" el;
      let id = get_attr_s el "id" in
      let hashval = 
	 Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) (id ^ password) in
      let hashtxt = 
	 Cryptokit.transform_string (Cryptokit.Hexa.encode ()) hashval in
	 out ("<handshake>" ^ hashtxt ^ "</handshake>");
	 let el = stream () in
print_endline (Xml.element_to_string el);
flush Pervasives.stdout;
	    match_tag "handshake" el;
	    send_xml out, stream
	       
let connect ?logfile server port =
   let inet_addr =
      try Unix.inet_addr_of_string server with Failure("inet_addr_of_string") ->
         (Unix.gethostbyname server).Unix.h_addr_list.(0) in
   let sock_addr = Unix.ADDR_INET (inet_addr, port) in
   let in_stream, out_stream = Unix.open_connection sock_addr in
      match logfile with
	 | Some file ->
              let f1, f2 = Unix.pipe () in
              let out_pipe = Unix.out_channel_of_descr f1 in
              let in_pipe = Unix.in_channel_of_descr f2 in
              let logfd = open_out_gen [Open_creat; Open_append] 0o666  file in
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
	 | None ->
              let send_raw text =
		 output_string out_stream text;
		 flush out_stream
              in
              let lexer = Xmlstream.parse_stream in_stream in
		 send_raw, lexer

let client ?logfile ~username ~password ~resource ?(port=5222) ~server () =
   let raw_out, lexer = connect ?logfile server port in
      open_stream_client raw_out lexer server username password resource

let service ?logfile server port username password =
   let raw_out, lexer = connect ?logfile server port in
      open_stream_service raw_out lexer server username password

(*********)

let get_bare_jid (jid:string) =
   try
      String.sub jid 0 (String.index jid '/')
   with Not_found -> jid

(*
   let r = Str.regexp "\\(\\(.+@\\)?[^/]+\\)\\(/.+\\)?"; in
   let t = Str.string_match r jid 0 in
      (* Strings.tolower ( *) Str.matched_group 1 jid (* ) *)
*)

let get_resource jid =
   try 
      let r = String.index jid '/' in
	 String.sub jid (r+1) (String.length jid - (r+1))
   with Not_found -> ""
(*
   let r = Str.regexp "/" in
      try
	 let pos = Str.search_forward r jid 0 in
	    Str.string_after jid (pos+1)
      with _ -> ""
*)

let jid_of_string str =
   let bare_jid, resource = 
      try
	 let r = String.index str '/' in
	 String.sub str 0 r,
	 String.sub str (r+1) (String.length str - (r+1))
      with Not_found -> str, ""
   in
   let user, host =
      try
	 let s = String.index bare_jid '@' in
	    String.sub bare_jid 0 s,
	    String.sub bare_jid (s+1) (String.length bare_jid - (s+1))
      with Not_found ->
	 "", bare_jid
   in
      user, host, resource

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

let iq_query ?subels ?to_ ?from ?id ?type_ xmlns =
   let attrs = 
      (match to_ with | Some v -> [("to", v)] | None -> []) @
	 (match from with | Some v -> [("from", v)] | None -> []) @
	 (match id with Some v -> [("id", v)] | None -> []) @
	 (match type_ with Some v -> [("type", v)] | None -> [("type", "get")])
   in
      Xmlelement ("iq", attrs,
		  [Xmlelement ("query", ["xmlns", xmlns], 
			       (match subels with
				   | None -> []
				   | Some s -> s
			       ))])

let make_presence ?(attrs=[]) ?(from="") ?(id="") ?(type_="")
   ?(status="") ?(subels=[])  to_ =
   let a = if attrs <> [] then attrs else
      Xml.filter_attrs
         [("from", from);
          ("to", to_); ("id", id); ("type", type_)] in

   let s1 = if status <> "" then [make_simple_cdata "status" status] else [] in
   let s2 = if subels <> [] then subels @ s1 else s1 in
      Xmlelement ("presence", a, s2)

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
		 if d <> "" then ("type", d) :: a4 else a4
