(*
 * (c) 2004, 2005, 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml

exception AuthError of string
exception Failure of string

let ns_sasl = "urn:ietf:params:xml:ns:xmpp-sasl"

let get_tag_by_ns= function
   | Xmlelement (tag, attrs, _) when 
	   (try List.assoc "xmlns" attrs with Not_found -> "") = ns_sasl -> tag
   | _ ->
	raise (AuthError "Invalid XMPProtocol")

let raise_failure = function
   | Xmlelement (_, _, els) ->
	let p = List.find (function
	   | Xmlelement (_, _, _) -> true
	   | _ -> false) els in
	   raise (Failure (get_tagname p))
   | _ ->
	raise NonXmlelement

let h s = Cryptokit.hash_string (Cryptokit.Hash.md5 ()) s
let hex s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s

let response_value ~username ~realm ~nonce ~cnonce ~qop ~nc ~digest_uri ~passwd =
   let a1 =  (h ((username ^ ":" ^ realm ^ ":" ^ passwd))) ^
      ":" ^ nonce ^ ":" ^ cnonce
   and a2 = "AUTHENTICATE:" ^ digest_uri in
   let t = (hex (h a1)) ^ ":" ^ nonce ^ ":" ^ nc ^ ":" ^ cnonce ^ ":" ^
      qop ^ ":" ^ (hex (h a2)) in
      hex (h t)

let make_cnonce () =
   let r = Array.init 8 (fun _ -> Char.chr(Random.int 256))
   in hex (Array.fold_left (fun a b -> a ^ (String.make 1 b)) "" r);;

let get_pairs str =
   let rec cycle data acc =
      let eq = String.index data '=' in
      let key = String.sub data 0 eq in
      let value, e =
	 if String.get data (eq+1) = '"' then
	    let q = String.index_from data (eq+2) '"' in
	       (String.sub data (eq+2) (q - (eq+2))), (q+1)
	 else
	    try 
	       let comma = String.index data ',' in
		  (String.sub data (eq+1) (comma - (eq+1))), (comma)
	    with Not_found -> 
	       (String.sub data (eq+1) (String.length data - (eq+1))), 
	       (String.length data)
      in 
      if e = String.length data then (key, value) :: acc
      else
	 let substr = String.sub data (e+1) (String.length data - (e+1)) in 
	    cycle substr ((key, value) :: acc)
   in 
      cycle str []

let sasl_digest_response chl username server passwd =
   let pairs = get_pairs (Cryptokit.transform_string
                             (Cryptokit.Base64.decode ()) chl) in
   let qop = List.assoc "qop" pairs
   and nonce = List.assoc "nonce" pairs
   and cnonce = make_cnonce ()
   and nc = "00000001"
   and digest_uri ="xmpp/" ^ server
   and realm = server in
   let response = response_value ~username ~realm
      ~nonce ~cnonce ~qop ~nc ~digest_uri ~passwd in
   let resp = Printf.sprintf "username=\"%s\",realm=\"%s\",nonce=\"%s\",cnonce=\"%s\",nc=%s,qop=%s,digest-uri=\"%s\",response=%s"
      username realm nonce cnonce nc qop digest_uri response
   in
      Cryptokit.transform_string (Cryptokit.Base64.encode_compact  ()) resp 
      ^ "=="

let sasl_digest_rspauth chl =
   let pairs = get_pairs (Cryptokit.transform_string
                             (Cryptokit.Base64.decode ()) chl) in
   let _rspauth = List.assoc "rspauth" pairs in
      ()

let sasl_digest next_xml out server username password =
   out (Xmlelement ("auth", ["xmlns", ns_sasl; "mechanism", "DIGEST-MD5"], []));
   let chl = next_xml () in
      if get_tag_by_ns chl = "challenge" then
	 let ch_text = Xml.get_cdata chl in
	 let resp = sasl_digest_response ch_text username server password in
	    out (Xmlelement ("response", ["xmlns", ns_sasl], [Xmlcdata resp]));
	    let p = next_xml () in
	       match get_tag_by_ns p with
		  | "failure" ->
		       raise_failure p
		  | "challenge" ->
		       sasl_digest_rspauth (get_cdata p);
		       out (Xmlelement ("response", ["xmlns", ns_sasl], []));
		       let s = next_xml () in
			  (match get_tag_by_ns s with
			     | "success" -> ()
			     | "failure" ->
				  raise_failure s
			     | _ ->
				  raise (AuthError "Invalid XMPProtocol")
			  )
		  | "success" ->
		       ()
		  | _ ->
		       raise (AuthError "Invalid XMPProtocol")
      else
	 raise (AuthError "Invalid XMPPotocol")

let sasl_plain next_xml out server username password =
   let sasl_data = 
      Cryptokit.transform_string (Cryptokit.Base64.encode_compact  ())
	 (Printf.sprintf "%s\x00%s\x00%s"
	       (username ^ "@" ^ server) username password)  ^ "==" in
      out (Xmlelement ("auth", ["xmlns", ns_sasl; "mechanism", "PLAIN"], 
                      [Xmlcdata sasl_data]));
      let p = next_xml () in
	 match get_tag_by_ns p with
	    | "failure" ->
		 raise_failure p
	    | "success" ->
		 ()
            | _ ->
		 raise (AuthError "Invalid XMPProtocol")

(*
let sasl_external next_xml out server username password =
   ()
*)
					  
let auth (next_xml:unit -> element) out mechanisms server username password =
   if List.mem "DIGEST-MD5" mechanisms then
      sasl_digest next_xml out server username password
   else if List.mem "PLAIN" mechanisms then
      sasl_plain next_xml out server username password
