(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

exception AuthError of string
exception Failure of string

let h s = Cryptokit.hash_string (Cryptokit.Hash.md5 ()) s
let hex s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s
    
let response_value ~username ~realm ~nonce ~cnonce ~qop ~nc ~digest_uri
    ~passwd =
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
      
