(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open XMPP_types
  
module Make (Network: NETWORK) =
struct
  
  open Network
  open Xmlstream
  open Light_xml

  module SASL = Sasl.Make(Network)

  exception Error of string
  exception StreamError of element list
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

  let rec next_xml inch p () =
    let newp, tag = Xmlstream.parse p in
      match tag with
        | Stanza (Xmlelement ("stream:error", _, els)) ->
            fail (StreamError els)
        | Stanza el ->
            return (newp, Stanza el)
        | StreamStart (name, attrs) ->
            return (newp, StreamStart (name, attrs))
        | StreamEnd ->
            fail End_of_file
        | Continue ->
            read inch >>=
              (fun buf ->
                 let newp = Xmlstream.add_buffer newp buf in
                   next_xml inch newp ())

  let open_stream_client server port username password resource =
    connect server port >>=
      (fun (inch, ouch) ->
         let rec check_stream_header (p, tag) =
           match tag with
             | StreamStart ("stream:stream", _attrs) ->
                 next_xml inch p ()
             | _ ->
                 fail (Error "bad protocol")
         and check_stream_features (p, tag) =
           match tag with
             | Stanza (Xmlelement ("stream:features", _, _) as el) ->
                 let mechanisms = get_tag el ["mechanisms"] in
                 let mels = get_subels ~tag:"mechanism" mechanisms in
                 let m = List.map
                   (function x -> get_cdata x) mels in
                   SASL.auth p (next_xml inch) (send_xml (send ouch))
                     m server username password >>=
                     new_stream
             | _ ->
                 fail (Error "hm")
         and new_stream _p =
           let p = Xmlstream.create () in
             send ouch (start_stream server) >>= 
               next_xml inch p >>= check_stream_header >>=
                 (fun (p, tag) ->
                    match tag with
                      | Stanza (Xmlelement ("stream:features", _, _) as el) ->
                          let _bind = get_tag el ["bind"] in
                            send_xml (send ouch)
                              (make_element "iq" ["type", "set";
                                                  "to", server; "id", "bind1"]
                                 [make_element "bind" ["xmlns",
                                             "urn:ietf:params:xml:ns:xmpp-bind"]
                                     [make_simple_cdata "resource" 
                                        resource]]) >>=
                              next_xml inch p >>= get_bind
                       | _ ->
                           fail (Error "no bind")
                  )
         and get_bind (p, tag) =
           match tag with
             | Stanza (Xmlelement ("iq", attrs, els) as el) ->
                 if get_attr_s el "type" = "result" &&
                   get_attr_s el "id" = "bind1" then
                     let myjid = get_cdata ~path:["bind";"jid"] el in
                       send_xml (send ouch)
                         (make_element "iq"
                            ["from", myjid; "type", "set"; "id", "session1"]
                            [make_element "session"
                               ["xmlns", "urn:ietf:params:xml:ns:xmpp-session"]
                               []]) >>= next_xml inch p >>= get_session myjid
                 else
                   fail (Error "Resource binding failed")
             | _ ->
                 fail (Error "Resource binding failed")
         and get_session myjid (p, tag) =
           match tag with
             | Stanza (Xmlelement ("iq", attrs, _) as el) ->
                 if get_attr_s el "type" = "result" &&
                   get_attr_s el "id" = "session1" then
                     return (myjid, p, inch, ouch)
                 else
                   fail (Error "Session binding failed")
             | _ ->
                 fail (Error "Session binding failed")
         in
         let p = Xmlstream.create () in
           send ouch (start_stream server) >>= next_xml inch p >>=
             check_stream_header >>= check_stream_features
      )
      
        
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
                                        | Xmlcdata _ -> false
                                   ) (get_subels xml) in
               try get_attr_s subel "xmlns" with Not_found -> 
                 raise  InvalidStanza)
        | `Result
        | `Error ->
            try
              let subel = List.find (fun i ->
                                       match i with
                                         | Xmlelement _ -> true
                                         | Xmlcdata _ -> false
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
                 | None -> make_element "iq" newattrs subels1
                 | Some news ->
                     match List.find (function
                                        | Xmlelement _ -> true
                                        | Xmlcdata _ -> false
                                     ) subels1 with
                       | Xmlelement (qn, qa, _) ->
                           make_element "iq" newattrs [make_element qn qa news]
                       | Xmlcdata _ -> raise NonXmlelement
            )
      | Xmlcdata _ -> raise NonXmlelement
          
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
      | Some x -> [make_element query_tag
                     (("xmlns", x) :: (match exattrs with
                                         | None -> []
                                         | Some ex -> ex))
                     (match subels with
                        | None -> []
                        | Some s -> s)]
    in
      make_element "iq" a4 s1
        
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
      make_element "presence" a5 s3
        
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
      make_element "message" a5 s3
        
end
