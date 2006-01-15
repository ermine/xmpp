(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml

exception AuthCreateError of string
exception AuthError

let h s = Cryptokit.hash_string (Cryptokit.Hash.md5 ()) s
let hex s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s

let response_value ~username_value ~realm_value ~nonce_value ~cnonce_value
   ~qop_value ~nc_value ~digest_uri_value ~passwd =
   let a1 =  (h ((username_value ^ ":" ^ realm_value ^ ":" ^ passwd))) ^
             ":" ^ nonce_value ^ ":" ^ cnonce_value
   and a2 = "AUTHENTICATE:" ^ digest_uri_value
   in
   let t = (hex (h a1)) ^ ":" ^
           nonce_value ^ ":" ^ nc_value ^ ":" ^ cnonce_value ^ ":" ^
           qop_value ^ ":" ^ (hex (h a2)) in
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

let sasl_digest_response chl username server password =

   let chld = Cryptokit.transform_string (Cryptokit.Base64.decode ()) chl in
   let pairs = get_pairs chld in
   let qop = List.assoc "qop" pairs
   and nonce = List.assoc "nonce" pairs
   and cnonce = make_cnonce ()
   and nc = "00000001"
   and digest_uri ="xmpp/" ^ server
   and realm = server in
   let response = response_value ~username_value:username
                     ~realm_value:server
                     ~nonce_value:nonce
                     ~cnonce_value:cnonce
                     ~qop_value:qop
                     ~nc_value:nc
                     ~digest_uri_value:digest_uri
                     ~passwd:password
   in
   let resp = Printf.sprintf "username=\"%s\",realm=\"%s\",nonce=\"%s\",cnonce=\"%s\",nc=%s,qop=%s,digest-uri=\"%s\",response=%s"
         username realm nonce cnonce nc qop digest_uri response
   in
      Cryptokit.transform_string (Cryptokit.Base64.encode_multiline  ()) resp

let sasl_digest_rspauth chl =
   let pairs = get_pairs (Cryptokit.transform_string
                             (Cryptokit.Base64.decode ()) chl) in
   let _rspauth = List.assoc "rspauth" pairs in
      ()

let sasl_digest out server username password next_xml =
   out (Xmlelement ("auth", ["xmlns", "urn:ietf:params:xml:ns:xmpp-sasl";
                             "mechanism", "DIGEST-MD5"], []));
   let chl = next_xml () in
      match_tag "challenge" chl;
      let ch_text = Xml.get_cdata chl in
      let resp = sasl_digest_response ch_text username server password in
	 out (Xmlelement ("response",
			  ["xmlns", "urn:ietf:params:xml:ns:xmpp-sasl"],
			  [Xmlcdata resp]));
	 let chl = next_xml () in
	    match Xml.get_tagname chl with
	       | "failure" ->
		    raise AuthError
	       | "challenge" ->
		    let chl_text = Xml.get_cdata chl in
		       sasl_digest_rspauth chl_text;
		       out (Xmlelement 
			       ("response",
				["xmlns", "urn:ietf:params:xml:ns:xmpp-sasl"], 
				[]));
		       let succ = next_xml () in
			  match_tag "success" succ
	       | _ ->
		    raise AuthError
					  
let sasl_auth mechanisms out server username password next_xml =
   if List.mem "DIGEST-MD5" mechanisms then
      sasl_digest out server username password next_xml
