(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xmlstream
open Light_xml

exception XMPPError of string
exception XMPPStreamEnd
exception XMPPStreamError of element list
exception InvalidStanza
exception InvalidProtocol

let send_xml out (el:element) = out (element_to_string el)

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

let open_stream_client send_raw read_raw server username password resource =
  send_raw (start_stream server);
  let stream next_xml =
    match next_xml () with
      | Element el -> el
      | StreamError els -> raise (XMPPStreamError els)
      | StreamEnd -> raise XMPPStreamEnd
  in
  let next_xml = Xmlstream.create read_raw in
  let el =  stream next_xml in match_tag "stream:stream" el;
    let el = stream next_xml in match_tag "stream:features" el;
      let mechanisms = get_tag el ["mechanisms"] in
      let mels = get_subels ~tag:"mechanism" mechanisms in
      let m = List.map (function x -> get_cdata x) mels in
        Sasl.auth (fun () -> stream next_xml) (send_xml send_raw)
          m server username password;
        let next_xml = Xmlstream.create read_raw in
          send_raw (start_stream server);
          let el = stream next_xml in
            match_tag "stream:stream" el;
            let el = stream next_xml in match_tag "stream:features" el;
              let _bind = get_tag el ["bind"] in
                send_xml send_raw 
                  (Xmlelement ("iq", ["type", "set";
                                      "to", server; "id", "bind1"],
                               [Xmlelement ("bind",
                                            ["xmlns",
                                             "urn:ietf:params:xml:ns:xmpp-bind"],
                                            [make_simple_cdata "resource" 
                                               resource])]));
                let el = stream next_xml in
                  if match_xml el "iq" 
                    ["type", "result"; "id", "bind1"] then
                      let myjid = get_cdata ~path:["bind";"jid"] el in
                        send_xml send_raw
                          (Xmlelement ("iq", 
                                       ["from", myjid; "type", "set";
                                        "id", "session1"],
                                       [Xmlelement ("session", 
                                                    ["xmlns",
                                                     "urn:ietf:params:xml:ns:xmpp-session"],
                                                    [])]));
                        let el = stream next_xml in
                          if match_xml el "iq" ["type", "result"; 
                                                "id","session1"] then
                            (myjid, send_xml send_raw,
                             (fun () -> stream next_xml))
                          else
                            raise 
                              (XMPPError "Session binding failed")
                  else
                    raise (XMPPError "Resource binding failed")
                    
let open_stream_service send_raw read_raw server name password =
  let stream next_xml =
    match next_xml () with
      | Element el -> el
      | StreamError els -> raise (XMPPStreamError els)
      | StreamEnd -> raise XMPPStreamEnd
  in
  let next_xml = Xmlstream.create read_raw in
    send_raw (start_stream ~streamtype:ComponentAccept server);
    let el =  stream next_xml in
      match_tag "stream:stream" el;
      let id = get_attr_s el "id" in
      let hashval = 
        Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) (id ^ password) in
      let hashtxt = 
        Cryptokit.transform_string (Cryptokit.Hexa.encode ()) hashval in
        send_raw ("<handshake>" ^ hashtxt ^ "</handshake>");
        let el = stream next_xml in
          match_tag "handshake" el;
          send_xml send_raw, stream
            
let connect ?rawxml_log server port =
  let inet_addr =
    try Unix.inet_addr_of_string server with Failure("inet_addr_of_string") ->
      (Unix.gethostbyname server).Unix.h_addr_list.(0) in
  let sock_addr = Unix.ADDR_INET (inet_addr, port) in
  let in_stream, out_stream = Unix.open_connection sock_addr in
  let logfd =
    match rawxml_log with
      | Some file ->
          let logfd = open_out_gen [Open_creat; Open_append] 0o666  file in
            Some logfd
      | None ->
          None
  in
  let send_raw =
    match logfd with
      | Some fd ->
          (fun raw ->
             Printf.fprintf fd "OUT: %s\n" raw;
             flush fd;
             output_string out_stream raw;
             flush out_stream
          )
      | None ->
          (fun raw ->
             output_string out_stream raw;
             flush out_stream
          )
  in
  let read_raw =
    match logfd with
      | Some fd ->
          (fun () ->
             let string = String.create 8193 in
             let size = input in_stream string 0 8192 in
               if size = 0 then (
                 Printf.fprintf fd "CLOSE\n";
                 flush fd;
                 ""
               ) else (
                 let str = String.sub string 0 size in
                   Printf.fprintf fd "IN: %s\n" str;
                   str
               )
          )
      | None ->
          (fun () ->
             let string = String.create 8193 in
             let size = input in_stream string 0 8192 in
               if size = 0 then (
                 close_in in_stream;
                 ""
               ) else
                 String.sub string 0 size
          )
  in
    send_raw, read_raw
              
let client ?rawxml_log ~username ~password ~resource ?(port=5222) ~server () =
  let send_raw, read_raw = connect ?rawxml_log server port in
    open_stream_client send_raw read_raw server username password resource
      
let service ?rawxml_log server port username password =
  let send_raw, read_raw = connect ?rawxml_log server port in
    open_stream_service send_raw read_raw server username password
      
(*********)
      
let get_xmlns xml =
  let subel = List.find (function
                           | Xmlelement (_, _, _) -> true
                           | Xmlcdata _ -> false
                        ) (get_subels xml) in
    get_attr_s subel "xmlns"
      
let make_attrs_reply ?lang ?type_ attrs =
  let to_ = try List.assoc "to" attrs with Not_found -> ""
  and from = try List.assoc "from" attrs with Not_found -> "" in

  let a1 = List.remove_assoc "to" attrs in
  let a2 = List.remove_assoc "from" a1 in

  let a3 = filter_attrs (("from", to_) :: ("to", from) :: a2) in
  let a4 = match lang with
    | None -> a3
    | Some l ->
        ("xml:lang", l) :: List.remove_assoc "xml:lang" a3 in
    match type_ with
      | None -> a4
      | Some d ->
          let a5 = List.remove_assoc "type" a4 in
          let a6 = if d <> "" then ("type", d) :: a5 else a5 in
            a6
              
type iq_type = [`Get | `Set | `Result | `Error]
    
let iq_info xml =
  let id = try get_attr_s xml "id" with Not_found -> "" in
  let type_ = match safe_get_attr_s xml "type" with
    | "get" -> `Get
    | "set" -> `Set
    | "result" -> `Result
    | "error" -> `Error
    | _ -> raise InvalidStanza in
  let xmlns =
    match type_ with
      | `Get
      | `Set -> 
          (let subel = List.find (fun i ->
                                    match i with
                                      | Xmlelement _ -> true
                                      | _ -> false
                                 ) (get_subels xml) in
             try get_attr_s subel "xmlns" with Not_found -> 
               raise  InvalidStanza)
      | `Result
      | `Error ->
          try
            let subel = List.find (fun i ->
                                     match i with
                                       | Xmlelement _ -> true
                                       | _ -> false
                                  ) (get_subels xml) in
              get_attr_s subel "xmlns"
          with Not_found -> ""
  in
    id, type_, xmlns
      
let iq_reply ?type_ ?lang ?subels xml =
  match xml with
    | Xmlelement (_, attrs, subels1) ->
        let newtype = match type_ with
          | None -> "result"
          | Some t ->
              match t with
                | `Result -> "result"
                | `Get -> "get"
                | `Set -> "set"
                | `Error -> "error"
        in
          (let newattrs = make_attrs_reply attrs ?lang ~type_:newtype in
             match subels with
               | None -> Xmlelement ("iq", newattrs, subels1)
               | Some news ->
                   match List.find (function
                                      | Xmlelement _ -> true
                                      | _ -> false
                                   ) subels1 with
                     | Xmlelement (qn, qa, _) ->
                         Xmlelement ("iq", newattrs,
                                     [Xmlelement (qn, qa, news)])
                     | _ -> raise NonXmlelement
          )
    | _ -> raise NonXmlelement
        
let make_iq ?to_ ?from ?(query_tag="query") ?xmlns ?exattrs ?subels ?lang ~id 
    ~type_ () =
  let a1 = [("id", id); ("type", match type_ with
                           | `Get -> "get"
                           | `Set -> "set"
                           | `Result -> "result"
                           | `Error -> "error")] in
  let a2 = match from with
    | None -> a1
    | Some f -> ["from", f] in
  let a3 = match to_ with
    | None -> a2
    | Some t -> ("to", t) :: a2 in
  let a4 = match lang with
    | None -> a3
    | Some l -> ("xml:lang", l) :: a3 in
    
  let s1 = match xmlns with
    | None -> []
    | Some x -> [Xmlelement (query_tag, ("xmlns", x) :: 
                               (match exattrs with
                                  | None -> []
                                  | Some ex -> ex),
                             match subels with
                               | None -> []
                               | Some s -> s)]
  in     
    Xmlelement ("iq", a4, s1)
      
type presence_show_t = [
| `Chat
| `Away
| `DND
| `XA
| `Online
]
    
type presence_t = [
| `Probe
| `Subscribe
| `Subscribed
| `Unsubscribe
| `Unsubscribed
| `Unavailable
| `Available of presence_show_t
| `Error
]

let presence_info xml =
  let t = match safe_get_attr_s xml "type" with
    | "probe" -> `Probe
    | "subscribe" -> `Subscribe
    | "subscribed" -> `Subscribed
    | "unsubscribe" -> `Unsubscribe
    | "unsubscribed" -> `Unsubscribed
    | "unavailable" -> `Unavailable
    | "error" -> `Error
    | _ ->
        let show = 
          match try get_cdata ~path:["show"] xml with Not_found -> "" with
            | "away" -> `Away
            | "xa" -> `XA
            | "dnd" -> `DND
            | "chat" -> `Chat
            | _ -> `Online
        in
          `Available show
  in
  let status = try get_cdata xml ~path:["status"] with 
      Not_found -> "" in
    t, status
      
let make_presence ?from ?to_ ?id ?type_ ?status ?subels ?lang () =
  let a1 = match from with
    | None -> []
    | Some f -> ["from", f] in
  let a2 = match to_ with
    | None -> a1
    | Some t -> ("to", t) :: a1 in
  let a3 = match id with
    | None -> a2
    | Some i -> ("id", i) :: a2 in
  let a4, s1 = match type_ with
    | None -> a3, []
    | Some p ->
        match p with
          | `Subscribe -> ("type", "subscribe") :: a3, []
          | `Subscribed -> ("type", "subscribed") :: a3, []
          | `Unsubscribe -> ("type", "unsubscribe") :: a3, []
          | `Unsubscribed -> ("type", "unsubscribed") :: a3, []
          | `Probe -> ("type", "probe") :: a3, []
          | `Error -> ("type", "error") :: a3, []
          | `Available show -> a3, 
              (match show  with
                 | None -> []
                 | Some `Chat -> [make_simple_cdata "chat" ""]
                 | Some `DND -> [make_simple_cdata "dnd" ""]
                 | Some `Away -> [make_simple_cdata "away" ""]
                 | Some `XA -> [make_simple_cdata "xa" ""])
          | `Unavailable -> ("type", "unavailable") :: a3, [] in
  let a5 = match lang with
    | None -> a4
    | Some l -> ("lang", l) :: a4 in
  let s2 = match status with
    | None -> s1
    | Some s -> make_simple_cdata "status" s :: s1 in
  let s3 = match subels with
    | None -> s2
    | Some s -> s2 @ s in
    Xmlelement ("presence", a5, s3)
      
type message_type = [
| `Normal
| `Chat
| `Groupchat
| `Headline
| `Error
]

let make_message ?from ?to_ ?type_ ?id ?subject ?body ?subels ?lang () =
  let a1 = match from with
    | None -> []
    | Some f -> ["from", f] in
  let a2 = match to_ with
    | None -> a1
    | Some t -> ("to", t) :: a1 in
  let a3 = match id with
    | None -> a2
    | Some i -> ("id", i) :: a2 in
  let a4 = match type_ with
    | None -> a3
    | Some t -> ("type", match t with
                   | `Normal -> "normal"
                   | `Chat -> "chat"
                   | `Groupchat -> "groupchat"
                   | `Headline -> "headline"
                   | `Error -> "error") :: a3 in
  let a5 = match lang with
    | None -> a4
    | Some l -> ("lang", l) :: a4 in
    
  let s1 = match subject with
    | None -> []
    | Some s -> [make_simple_cdata "subject" s] in
  let s2 = match body with
    | None -> s1
    | Some b -> make_simple_cdata "body" b :: s1 in
  let s3 = match subels with
    | None -> s2
    | Some s -> s2 @ s in
    Xmlelement ("message", a5, s3)
      
