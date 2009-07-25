(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open XMPP_types
  
module Make (Network: NETWORK) =
struct
  
  open Network
  open Xmlstream
  open Xml

  module SASL = Sasl.Make(Network)

  exception Error of string
  exception StreamError of StreamError.t
  exception MalformedStanza
  exception InvalidProtocol

  let ns_streams = Some "http://etherx.jabber.org/streams"
  let ns_server = Some "jabber:server"
  let ns_client = Some "jabber:client"
  let ns_tls = Some "urn:ietf:params:xml:ns:xmpp-tls"
  let ns_sasl = Some "urn:ietf:params:xml:ns:xmpp-sasl"
  let ns_bind = Some "urn:ietf:params:xml:ns:xmpp-bind"
  let ns_session = Some "urn:ietf:params:xml:ns:xmpp-session"

  let get_type el = get_attr_value "type" (get_attrs el)
  let get_id el = get_attr_value "id" (get_attrs el)
  let get_from el = get_attr_value "from" (get_attrs el)
  let get_to el = get_attr_value "to" (get_attrs el)
  let get_lang el = get_attr_value ~ns:ns_xml "lang" (get_attrs el)
    
  let rec next_xml inch p () =
    match Xmlstream.parse p with
      | Stanza el ->
          if get_qname el = (ns_streams, "error") then
            fail (StreamError (StreamError.parse_error el))
          else
            return (Stanza el)
      | StreamStart (name, attrs) ->
          return (StreamStart (name, attrs))
      | StreamEnd ->
          fail End_of_file
      | Continue ->
          read inch >>=
            (fun buf ->
               Xmlstream.add_buffer p buf;
               next_xml inch p ())
              
  let open_stream_client server port username password resource =
    connect server port >>=
      (fun (inch, ouch) ->
         let p = Xmlstream.create [ns_streams; ns_client] in
         let () = Xmlstream.bind_prefix p "stream" ns_streams in
         let rec check_stream_header = function
           | StreamStart (qname, attrs) ->
               if qname = (ns_streams, "stream") &&
                 get_attr_value "version" attrs  = "1.0" then
                   next_xml inch p ()
               else
                 fail (Error "bad stream header")
           | _ ->
               fail (Error "bad protocol")
         and check_stream_features = function
           | Stanza (Xmlelement (qname, _, _) as el) ->
               print_endline "start stream";
               if qname = (ns_streams, "features") then
                 let mechanisms = get_subelement (ns_sasl, "mechanisms") el in
                 let mels = get_subelements (ns_sasl, "mechanism") mechanisms in
                 let m = List.map get_cdata mels in
                   SASL.auth p (next_xml inch) (send ouch)
                     m server username password >>=
                     new_stream
               else
                 fail (Error "hm")
           | _ ->
               fail (Error "hm")
         and new_stream _p =
           let p = Xmlstream.create [ns_streams; ns_client] in
           let () = bind_prefix p "stream" ns_streams in
             send ouch (Xmlstream.stream_header p (ns_streams, "stream")
                          [make_attr "to" server; make_attr "version" "1.0"]) >>=
               next_xml inch p >>= check_stream_header >>=
                 (function
                    | Stanza el ->
                        if get_qname el = (ns_streams, "features") &&
                          mem_child (ns_bind, "bind") el then
                            send ouch
                              (Xmlstream.stanza_serialize p
                                 (make_element (ns_client, "iq")
                                    [make_attr "type" "set";
                                     make_attr "to" server;
                                     make_attr "id" "bind1"]
                                    [make_element (ns_bind, "bind")
                                       []
                                       [make_simple_cdata
                                          (ns_client, "resource") 
                                          resource]])) >>=
                              next_xml inch p >>= get_bind
                        else
                          fail (Error "no bind")
                    | _ ->
                        fail (Error "no bind")
                 )
         and get_bind = function
           | Stanza (Xmlelement (qname, attrs, els) as el) ->
               if qname = (ns_client, "iq") &&
                 safe_get_attr_value "type" attrs = "result" &&
                 safe_get_attr_value "id" attrs = "bind1" then
                   let myjid = get_cdata
                     (get_subelement (ns_bind, "jid")
                        (get_element (ns_bind, "bind") els)) in
                     send ouch
                       (Xmlstream.stanza_serialize p
                          (make_element (ns_client, "iq")
                             [make_attr "from" myjid;
                              make_attr "type" "set";
                              make_attr "id" "session1"]
                             [make_element (ns_session, "session") [] []])
                       ) >>= next_xml inch p >>= get_session myjid
               else
                 fail (Error "Resource binding failed")
           | _ ->
               fail (Error "Resource binding failed")
         and get_session myjid = function
           | Stanza (Xmlelement (qname, attrs, _)) ->
               if qname = (ns_client, "iq") &&
                 get_attr_value "type" attrs = "result" &&
                 get_attr_value "id" attrs = "session1" then
                   return (myjid, p, inch, ouch)
               else
                 fail (Error "Session binding failed")
           | _ ->
               fail (Error "Session binding failed")
         in
           send ouch (stream_header p (ns_streams, "stream")
                        [make_attr "to" server;
                         make_attr "version" "1.0"]) >>=
             next_xml inch p >>=
               check_stream_header >>=
                 check_stream_features
      )
        
        
  (*********)
                          
  let make_attrs_reply ?lang ?type_ attrs =
    let jid_to = safe_get_attr_value "to" attrs
    and jid_from = safe_get_attr_value "from" attrs
    and id = safe_get_attr_value "id" attrs 
    and type_ = (
      match type_ with
        | None -> safe_get_attr_value "type" attrs
        | Some v -> v)
    and lang = (
      match lang with
        | None -> safe_get_attr_value ~ns:ns_xml "lang" attrs
        | Some v -> v) in
      List.fold_left (fun acc (k,v) ->
                        if v <> "" then
                          (make_attr k v) :: acc
                        else
                          acc
                     )
        (if lang = "" then [] else [make_attr ~ns:ns_xml "lang" lang])
        ["to", jid_from;
         "from", jid_to;
         "id", id;
         "type", type_]
        
  type id = string
      
  type iq_type = [`Get | `Set | `Result | `Error]

  let string_of_iq_type = function
    | `Get -> "get"
    | `Set -> "set"
    | `Result -> "result"
    | `Error -> "error"

  let iq_type_of_string = function
    | "get" -> `Get
    | "set" -> `Set
    | "result" -> `Result
    | "error" -> `Error
    | _ -> raise (Error "invalid iq type")

  type iq_info =
    | IqSet of element
    | IqGet of element
    | IqResult of element option
    | IqError of StanzaError.t
        
  let iq_info = function
    | Xmlelement (qname, attrs, els) -> (
        match safe_get_attr_value "type" attrs with
          | "get" ->
              IqGet (get_first_element els)
          | "set" ->
              IqSet (get_first_element els)
          | "result" ->
              IqResult (try Some (get_first_element els) with Not_found -> None)
          | "error" -> (
              try
                let el = get_element (ns_client, "error") els in
                  IqError (StanzaError.parse_error el)
              with Not_found -> raise MalformedStanza
            )
          | _ -> raise MalformedStanza
      )
    | Xmlcdata _ -> raise NonXmlelement
        
  let make_iq_reply ?(type_=`Result) ?lang ?payload = function
    | Xmlelement (qname, attrs, els) ->
        let newtype = string_of_iq_type type_ in
        let newattrs = make_attrs_reply attrs ?lang ~type_:newtype in
          (match payload with
             | None -> make_element qname newattrs els
             | Some newels -> make_element qname newattrs newels)
    | Xmlcdata _ -> raise NonXmlelement
    
  let make_iq ~ns ~id ?jid_to ?jid_from ?payload ?lang ~type_ () =
    let attrs =
      List.fold_left (fun acc (k,v) ->
                        if v <> "" then
                          make_attr k v :: acc
                        else
                          acc
                     )
        (match lang with
           | None -> []
           | Some v -> [make_attr ~ns:ns_xml "lang" v])
        ["to",  (match jid_to with | None -> "" | Some v -> v);
         "from", (match jid_from with | None -> "" | Some v -> v);
         "id", id;
         "type", string_of_iq_type type_]
    in
      make_element (ns, "iq") attrs
        (match payload with
           | None -> []
           | Some v -> v)
        
        
  type presence_show_t = [
  | `Chat
  | `Away
  | `DND
  | `XA
  | `Online
  ]

  type presence_type = [
  | `Probe
  | `Subscribe
  | `Subscribed
  | `Unsubscribe
  | `Unsubscribed
  | `Unavailable
  | `Available
  | `Error
  ]

  let string_of_presence_type = function
    | `Probe -> "probe"
    | `Subscribe -> "subscribe"
    | `Subscribed -> "subscribed"
    | `Unsubscribe -> "unsubscribe"
    | `Unsubscribed -> "unsubscribed"
    | `Unavailable -> "unavailable"
    | `Available -> ""
    | `Error -> "error"

  let presence_type_of_string = function
    | "probe" -> `Probe
    | "subscribe" -> `Subscribe
    | "subscribed" -> `Subscribed
    | "unsubscribe" -> `Unsubscribe
    | "unsubscribed" -> `Unsubscribed
    | "unavailable" -> `Unavailable
    | "" -> `Available
    | "error" -> `Error
    | _ -> raise (Error "invalid presence type")
      
  let get_presence_type = function
    | Xmlelement (_qname, attrs, _els) ->
        presence_type_of_string (safe_get_attr_value "type" attrs)
    | Xmlcdata _ ->
        raise NonXmlelement

  let get_presence_show ~ns = function
    | Xmlelement (_, _, els) -> (
        match try get_cdata (get_element (ns, "show") els) with Not_found -> ""
        with
          | "away" -> `Away
          | "xa" -> `XA
          | "dnd" -> `DND
          | "chat" -> `Chat
          | _ -> `Online              
      )
    | Xmlcdata _ ->
        raise NonXmlelement

  let get_presence_status ~ns = function
    | Xmlelement (_qname, _attrs, els) -> (
        try get_cdata (get_element (ns, "status") els) with Not_found -> ""
      )
    | Xmlcdata _ ->
        raise NonXmlelement
        
  let make_presence ~ns ?jid_from ?jid_to ?id ?type_
      ?show ?status ?priority ?(subels=[]) ?lang () =
    let attrs =
      List.fold_left (fun acc (k,v) ->
                        if v <> "" then
                          make_attr k v :: acc
                        else
                          acc
                     )
        (match lang with
           | None -> []
           | Some v -> [make_attr ~ns:ns_xml "lang" v])
        ["to",  (match jid_to with | None -> "" | Some v -> v);
         "from", (match jid_from with | None -> "" | Some v -> v);
         "id", (match id with | None -> "" | Some v -> v);
         "type", (match type_ with
                    | None -> ""
                    | Some v ->string_of_presence_type v)]
    in
    let els =
      List.fold_left (fun acc (k,v) ->
                        match v with
                          | None -> acc
                          | Some v -> make_simple_cdata (ns, k) v :: acc
                     ) subels
        ["show", (match show with
                    | None -> None
                    | Some v ->
                        match v with
                          | `Chat -> Some "chat"
                          | `Away -> Some "away"
                          | `DND -> Some "dnd"
                          | `XA -> Some "xa"
                          | `Online -> None);
         "status", status;
         "priority", (match priority with
                        | None -> None
                        | Some i -> Some (string_of_int i))] in
      make_element (ns, "presence") attrs els
        
  type message_type = [
  | `Normal
  | `Chat
  | `Groupchat
  | `Error
  ]

  let string_of_message_type = function
    | `Normal -> "normal"
    | `Chat -> "chat"
    | `Groupchat -> "groupchat"
    | `Error -> "error"

  let make_message ~ns ?jid_from ?jid_to ?type_ ?id ?subject ?body ?thread
      ?(subels=[]) ?lang () =
    let attrs =
      List.fold_left (fun acc (k,v) ->
                        if v <> "" then
                          make_attr k v :: acc
                        else
                          acc
                     )
        (match lang with
           | None -> []
           | Some v -> [make_attr ~ns:ns_xml "lang" v])
        ["to",  (match jid_to with | None -> "" | Some v -> v);
         "from", (match jid_from with | None -> "" | Some v -> v);
         "id", (match id with | None -> "" | Some v -> v);
         "type", (match type_ with
                    | None -> ""
                    | Some v -> string_of_message_type v)]
    in
    let els =
      List.fold_left (fun acc (k, v) ->
                        match v with
                          | None -> acc
                          | Some v -> make_simple_cdata (ns, k) v :: acc
                     ) subels
        ["subject", subject;
         "body", body;
         "thread", thread]
    in
      make_element (ns, "message") attrs els

  let make_error_reply condition ?text ?lang = function
    | Xmlelement ((ns, name), attrs, els) ->
        let error = StanzaError.make_error ~ns ?text ?lang condition in
        let newattrs = make_attrs_reply ~type_:"error" attrs in
          make_element (ns, name) newattrs (error::els)
    | Xmlcdata _ -> raise MalformedStanza
end
