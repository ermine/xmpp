(*
 * (c) 2004-2010 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *
 * RFC 2831 Digest SASL Mechanism
 * 
 *)

exception Error of string
exception Failure of string

type t =
  | Token of string
  | Separator of char
      
let separators = ['('; ')'; '<'; '>'; '@';
                  ','; ';'; ':'; '\\'; '"';
                  '/'; '['; ']'; '?'; '=';
                  '{'; '}'; ' '; '\t'
                 ]

let is_ctl ch =
  match ch with
    | '\000'..'\031' -> true
    | '\127' -> true
    | _ -> false
  
let make_lexer =
  let buf = Buffer.create 100 in
  let rec tokenizer strm =
    match Stream.peek strm with
      | Some ch ->
          if ch = '"' then (
            Stream.junk strm;
            get_string strm
          ) else if ch = ' ' || ch = '\t' || is_ctl ch then (
            Stream.junk strm;
            tokenizer strm
          ) else if List.mem ch separators then (
            Stream.junk strm;
            Some (Separator ch)
          ) else
            get_token strm
      | None -> None
  and get_string strm =
    match Stream.peek strm with
      | Some ch ->
          if ch = '"' then (
            Stream.junk strm;
            let str = Buffer.contents buf in
              Buffer.reset buf;
              Some (Token str)
          ) else if ch = '\\' then (
            Stream.junk strm;
            match Stream.peek strm with
              | Some ch1 ->
                  Stream.junk strm;
                  Buffer.add_char buf ch1;
                  get_string strm
              | None ->
                  failwith "Unterminated string"
          )
          else (
            Stream.junk strm;
            Buffer.add_char buf ch;
            get_string strm
          )
      | None ->
          failwith "Unterminated string"
  and get_token strm =
    match Stream.peek strm with
      | Some ch ->
          if List.mem ch separators || is_ctl ch then
            let str = Buffer.contents buf in
              Buffer.reset buf;
              Some (Token str)
          else (
            Stream.junk strm;
            Buffer.add_char buf ch;
            get_token strm
          )
      | None ->
          let str = Buffer.contents buf in
            Buffer.reset buf;
            Some (Token str)
  in
    fun strm -> Stream.from (fun _ -> tokenizer strm)
            
let get_pairs str =
  let rec scan acc = parser
  | [< 'Token t1; 'Separator '='; 'Token t2; rest >] ->
      check_comma ((t1, t2) :: acc) rest
and check_comma acc = parser
  | [< 'Separator ','; rest >] ->
      scan acc rest
  | [< >] ->
      List.rev acc
  in
  let strm = make_lexer (Stream.of_string str) in
    try
      scan [] strm
    with _ -> raise (Error "Malformed SASL challenge")
        
let parse_qop str =
  let rec qop acc = parser
    | [< 'Token t; rest >] ->
        check_comma (t :: acc) rest
    | [< >] ->
        List.rev acc
  and check_comma acc = parser
    | [< 'Separator ','; rest >] ->
        qop acc rest
    | [< >] ->
        List.rev acc
  in
  let strm = make_lexer (Stream.of_string str) in
    try
      qop [] strm
    with _ ->
      raise (Error "Malformed qop in SASL challenge")

let h s = Cryptokit.hash_string (Cryptokit.Hash.md5 ()) s
let hex s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s
  
let response_value ~username ~realm ~nonce ~cnonce ~qop ~nc ~digest_uri ~passwd =
  let a1 =
    (h (username ^ ":" ^ realm ^ ":" ^ passwd)) ^ ":" ^ nonce ^ ":" ^ cnonce
  and a2 = "AUTHENTICATE:" ^ digest_uri in
  let t = (hex (h a1)) ^ ":" ^ nonce ^ ":" ^ nc ^ ":" ^ cnonce ^ ":" ^
    qop ^ ":" ^ (hex (h a2)) in
    hex (h t)

let make_cnonce () =
  let r = Array.init 8 (fun _ -> Char.chr(Random.int 256)) in
    hex (Array.fold_left (fun a b -> a ^ (String.make 1 b)) "" r)

let parse_digest_md5_challenge str =
  let pairs = get_pairs str in
    try
      let qop = parse_qop (List.assoc "qop" pairs)
      and nonce = List.assoc "nonce" pairs in
        (*
      and realm = List.assoc "realm" pairs in
        (realm, qop, nonce)
        *)
        (qop, nonce)
    with Not_found ->
      raise (Error "Malformed SASL challenge")
  
let sasl_digest_response chl username server passwd =
  let str = Base64.decode chl in
  let () =
    print_endline str;
    flush stdout in
  let qop, nonce = parse_digest_md5_challenge str
  and cnonce = make_cnonce ()
  and nc = "00000001"
  and digest_uri ="xmpp/" ^ server
  and realm = server in
    if List.mem "auth" qop then
      let qop_method = "auth" in
      let response = response_value ~username ~realm
        ~nonce ~cnonce ~nc ~qop:qop_method ~digest_uri ~passwd in
      let resp = Printf.sprintf
        "charset=utf-8,username=\"%s\",realm=\"%s\",nonce=\"%s\",cnonce=\"%s\",nc=%s,qop=\"%s\",digest-uri=\"%s\",response=%s"
        username realm nonce cnonce nc qop_method digest_uri response
      in
        print_endline resp;
        flush stdout;
        Base64.encode resp
    else
      raise (Error "No known qop methods")

let sasl_digest_rspauth chl =
  let str = Base64.decode chl in
  let pairs = get_pairs str in
  let _rspauth = List.assoc "rspauth" pairs in
    ()
