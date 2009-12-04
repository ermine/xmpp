(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open Xmlstream
open StanzaError
open Jid
open Transport  

exception Error of string
exception StreamError of StreamError.t
exception MalformedStanza
exception BadRequest

type id = string

module ID =
struct
  type t = id
  let compare = compare
end
module IDCallback = Treap.Map(ID)

module NS =
struct
  type t = Xml.namespace
  let compare = compare
end
module IQRequestCallback = Map.Make(NS)

module Qname =
struct
  type t = Xml.qname
  let compare = compare
end
module StanzaHandler =Map.Make(Qname)

let ns_streams = Some "http://etherx.jabber.org/streams"
let ns_server = Some "jabber:server"
let ns_client = Some "jabber:client"
let ns_xmpp_tls = Some "urn:ietf:params:xml:ns:xmpp-tls"
let ns_xmpp_sasl = Some "urn:ietf:params:xml:ns:xmpp-sasl"
let ns_xmpp_bind = Some "urn:ietf:params:xml:ns:xmpp-bind"
let ns_xmpp_session = Some "urn:ietf:params:xml:ns:xmpp-session"

type iq_request =
  | IQSet of element
  | IQGet of element
        
type iq_response =
  | IQResult of element option
  | IQError of StanzaError.t
        
type iq =
  | IQRequest of iq_request
  | IQResponse of iq_response

type 'a t = {
  socket : Transport.t;
  mutable sid : int;
  xmllang : string;
  mutable iq_response :
    (iq_response -> string option -> string option -> string option -> unit ->
       unit) IDCallback.t;
  mutable iq_request : (iq_request -> string option -> string option ->
                          string option -> unit ->
                            iq_response) IQRequestCallback.t;
  mutable stanza_handlers : ('a t -> Xml.qname -> Xml.attribute list ->
                               Xml.element list -> unit) StanzaHandler.t;
  mutable myjid : jid;
  p : Xmlstream.t;
  data : 'a
}

let string_of_option opt = match opt with None -> "" | Some v -> v
let maybe f = function None -> None | Some v -> Some (f v)

let make_stanza_attrs ?id ?jid_from ?jid_to ?kind ?lang () =
    List.fold_left
      (fun acc (k, v) ->
         if v = "" then acc else make_attr k v :: acc
      ) (match lang with
           | None -> []
           | Some v -> [make_attr ~ns:ns_xml "lang" v])
      ["id", (string_of_option id);
       "to", (string_of_option jid_to);
       "from", (string_of_option jid_from);
       "type", (string_of_option kind)]

let make_stanza_attrs_reply ?lang ?kind attrs =
  let jid_to = safe_get_attr_value "to" attrs
  and jid_from = safe_get_attr_value "from" attrs
  and id = safe_get_attr_value "id" attrs 
  and kind = (
    match kind with
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
       "type", kind]

let parse_stanza_attrs attrs =
  List.fold_left (fun (id, from, to_, kind, lang) (name, value) ->
                    if name = (no_ns, "id") then
                      (Some value, from, to_, kind, lang)
                    else if name = (no_ns, "from") then
                      (id, Some value, to_, kind, lang)
                    else if name = (no_ns, "to") then
                      (id, from, Some value, kind, lang)
                    else if name = (no_ns, "type") then
                      (id, from, to_, Some value, lang)
                    else if name = (ns_xml, "lang") then
                      (id, from, to_, kind, Some value)
                    else
                      (id, from, to_, kind, lang)
                 ) (None, None, None, None, None) attrs
      
let make_iq_request t ?jid_from ?jid_to ?lang request callback =
  t.sid <- t.sid + 1;
  let id = string_of_int t.sid ^ ":" ^ string_of_int (Random.int 1000) in
  let kind, el =
    match request with
      | IQSet el -> "set", el
      | IQGet el -> "get", el
  in
  let jid_to = maybe string_of_jid jid_to in
  let jid_from = maybe string_of_jid jid_from in
  let attrs = make_stanza_attrs ~id ~kind ?jid_from ?jid_to ?lang () in
    t.iq_response <- IDCallback.add t.iq_response id callback 1;
    t.socket.send (Xmlstream.stanza_serialize t.p
                     (make_element (ns_client, "iq") attrs [el]))
      
type 'a stanza = {
  id : id option;
  jid_from : Jid.jid option;
  jid_to : string option;
  lang : string option;
  content : 'a;
  x : element list
}

type message_type =
  | Normal
  | Chat
  | Groupchat
  | Headline

type message_content = {
  message_type : message_type option;
  body : string option;
  subject : string option;
  thread : string option
}

let string_of_message_type = function
  | Normal -> "normal"
  | Chat -> "chat"
  | Groupchat -> "groupchat"
  | Headline -> "headline"
    
type message_stanza = message_content stanza

let parse_message ~callback ~callback_error t _qname attrs els =
  let id, jid_from, jid_to, kind, lang = parse_stanza_attrs attrs in
  let jid_from = maybe jid_of_string jid_from in
    if kind = Some "error" then
      let err = StanzaError.parse_error
        (get_element (ns_client, "error") els) in
        callback_error t ?id ?jid_from ?jid_to ?lang err
    else      
      let kind =
        match kind with
          | Some v -> (
              match v with
                | "normal" -> Some Normal
                | "chat" -> Some Chat
                | "groupchat" -> Some Groupchat
                | "headline" -> Some Headline
                | _ -> Some Normal
            )
          | None -> None
      in
      let x, body, subject, thread =
        List.fold_left (fun (x, body, subject, thread) -> function
                          | Xmlelement (qname, _attrs, els) as el->
                              if qname = (ns_client, "body") then
                                let body = collect_cdata els in
                                  (x, Some body, subject, thread)
                              else if qname = (ns_client, "subject") then
                                let subject = collect_cdata els in
                                  (x, body, Some subject, thread)
                              else if qname = (ns_client, "thread") then
                                let thread = collect_cdata els in
                                  (x, body, subject, Some thread)
                              else
                                (el :: x, body, subject, thread)
                          | Xmlcdata _ ->
                              (x, body, subject, thread)
                       ) ([], None, None, None) els
      in
      let message_stanza = {
        id = id;
        jid_from = jid_from;
        jid_to = jid_to;
        lang = lang;
        content = { message_type = kind;
                    body = body;
                    subject = subject;
                    thread = thread
                  };
        x = x
      }
      in
        callback t message_stanza
      
let send_message t ?id ?jid_from ?jid_to ?kind ?lang
    ?body ?subject ?thread ?(x=[]) () =
  let jid_to = maybe string_of_jid jid_to in
  let jid_from = maybe string_of_jid jid_from in
  let kind = maybe string_of_message_type kind in
  let attrs = make_stanza_attrs ?id ?jid_from ?jid_to ?kind?lang () in
  let els = List.fold_left (fun acc (k, v) ->
                              match v with
                                | None -> acc
                                | Some v ->
                                    make_simple_cdata (ns_client, k) v :: acc
                           ) x ["body", body;
                                "subject", subject;
                                "thread", thread] in
    t.socket.send (Xmlstream.stanza_serialize t.p
                     (make_element (ns_client, "message") attrs els))
    
type presence_type =
  | Probe
  | Subscribe
  | Subscribed
  | Unsubscribe
  | Unsubscribed
  | Unavailable

let string_of_presence_type = function
  | Probe -> "probe"
  | Subscribe -> "subscribe"
  | Subscribed -> "subscribed"
  | Unsubscribe -> "unsubscribe"
  | Unsubscribed -> "unsubscribed"
  | Unavailable -> "unavailable"

type presence_show =
  | ShowChat
  | ShowAway
  | ShowDND
  | ShowXA

let string_of_show = function
  | ShowChat -> "chat"
  | ShowAway -> "away"
  | ShowDND -> "dnd"
  | ShowXA -> "xa"
      
type presence_content = {
  presence_type : presence_type option;
  show : presence_show option;
  status : string option;
  priority : int option
}

type presence_stanza = presence_content stanza

let parse_presence ~callback ~callback_error t _qname attrs els =
  let id, jid_from, jid_to, kind, lang = parse_stanza_attrs attrs in
  let jid_from = maybe jid_of_string jid_from in
    if kind = Some "error" then
      let err = StanzaError.parse_error
        (get_element (ns_client, "error") els) in
        callback_error t ?id ?jid_from ?jid_to ?lang err
    else      
      let kind =
        match kind with
          | None -> None
          | Some v ->
              match v with
                | "probe" -> Some Probe
                | "subscribe" -> Some Subscribe
                | "subscribed" -> Some Subscribed
                | "unsubscribe" -> Some Unsubscribe
                | "unsubscribed" -> Some Unsubscribed
                | "unavailable" -> Some Unavailable
                | _ -> None
      in
      let x, show, status, priority =
        List.fold_left (fun (x, show, status, priority) -> function
                          | Xmlelement (qname, _attrs, els) as el ->
                              if qname = (ns_client, "show") then
                                let show = collect_cdata els in
                                  (x, Some show, status, priority)
                              else if qname = (ns_client, "status") then
                                let status = collect_cdata els in
                                  (x, show, Some status, priority)
                              else if qname = (ns_client, "priority") then
                                let priority =
                                  try Some (int_of_string (collect_cdata els))
                                  with _ -> None in
                                  (x, show, status, priority)
                              else
                                (el :: x, show, status, priority)
                          | Xmlcdata _ ->
                              (x, show, status, priority)
                       ) ([], None, None, None) els
      in
      let show =
        match show with
          | None -> None
          | Some v ->
              match v with
                | "chat" -> Some ShowChat
                | "dnd" -> Some ShowDND
                | "away" -> Some ShowAway
                | "xa" -> Some ShowXA
                | _ -> None
      in
      let presence_stanza = {
        id = id;
        jid_from = jid_from;
        jid_to = jid_to;
        lang = lang;
        content = { presence_type = kind;
                    show = show;
                    status = status;
                    priority = priority
                  };
        x = x
      }
      in
        callback t presence_stanza
      
let send_presence t ?id ?jid_from ?jid_to ?kind ?lang
    ?show ?status ?priority ?(x=[]) () =
  let jid_to = maybe string_of_jid jid_to in
  let jid_from = maybe string_of_jid jid_from in
  let kind = maybe string_of_presence_type kind in
  let attrs = make_stanza_attrs ?id ?jid_from ?jid_to ?kind ?lang () in
  let els =
    List.fold_left (fun acc (k,v) ->
                      match v with
                        | None -> acc
                        | Some v -> make_simple_cdata (ns_client, k) v :: acc
                   ) x
      ["show", (maybe string_of_show show);
       "status", status;
       "priority", (maybe string_of_int priority)] in
    t.socket.send (Xmlstream.stanza_serialize t.p
                     (make_element (ns_client, "presence") attrs els))
      
let send_error_reply t condition ?text ?lang qname attrs els =
  let ns = get_namespace qname in
  let error = make_error ~ns ?text ?lang condition in
  let newattrs = make_stanza_attrs_reply ~kind:"error" attrs in
    t.socket.send (Xmlstream.stanza_serialize t.p
                     (make_element qname newattrs (error::els)))
      
let register_iq_request_handler t namespace handler =
  t.iq_request <- IQRequestCallback.add namespace handler t.iq_request

let unregister_iq_request_handler t namespace =
  t.iq_request <- IQRequestCallback.remove namespace t.iq_request

let register_stanza_handler t qname handler =
  t.stanza_handlers <- StanzaHandler.add qname handler t.stanza_handlers

let unregister_stanza_handler t qname =
  t.stanza_handlers <- StanzaHandler.remove qname t.stanza_handlers
    
let process_iq t _qname attrs els =
  let id, jid_from, jid_to, kind, lang = parse_stanza_attrs attrs in
  let event =
    match kind with
      | Some "get" ->
          let payload =
            try get_first_element els with Not_found -> raise BadRequest in
            IQRequest (IQGet payload)
      | Some "set" ->
          let payload =
            try get_first_element els with Not_found -> raise BadRequest in
            IQRequest (IQSet payload)
      | Some "result" ->
          let el = try Some (get_first_element els) with Not_found -> None in
            IQResponse (IQResult el)
      | Some "error" ->
          let el =
            try get_element (ns_client, "error") els
            with Not_found -> raise MalformedStanza in
            IQResponse (IQError (StanzaError.parse_error el))
      | _ -> raise BadRequest
  in
    match event with
      | IQRequest ((IQSet el) as ev)
      | IQRequest ((IQGet el) as ev) -> (
          let namespace = get_namespace (get_qname el) in
          let proc =
            try Some (IQRequestCallback.find namespace t.iq_request)
            with Not_found -> None in
          let response =
            match proc with
              | None ->
                  IQError (create_error ERR_FEATURE_NOT_IMPLEMENTED)
              | Some f ->
                  f ev jid_from jid_to lang ()
          in
          let xml =
            match response with
              | IQResult el ->
                  make_element (ns_client, "iq")
                    (make_stanza_attrs ?id ~kind:"result" ?lang
                       ?jid_from:jid_to ?jid_to:jid_from ())
                    (match el with None -> [] | Some el -> [el])
              | IQError err ->
                  make_element (ns_client, "iq")
                    (make_stanza_attrs ?id ~kind:"error" ?lang
                       ?jid_from:jid_to ?jid_to:jid_from ())
                    [of_error ns_client err]
          in
            t.socket.send (Xmlstream.stanza_serialize t.p xml)
        )
      | IQResponse ev -> (
          match id with
            | None ->
                raise BadRequest
            | Some id -> 
                try
                  let (f, _) = IDCallback.find t.iq_response id in
                    t.iq_response <- IDCallback.delete t.iq_response id;
                    f ev jid_from jid_to lang ()
                with Not_found -> ()
        )
          
let make_session t session =
  make_iq_request t ~jid_from:t.myjid
    (IQSet (make_element (ns_xmpp_session, "session") [] []))
    (fun ev _jid_from _jid_to _lang () ->
       match ev with
         | IQResult _ -> session t
         | IQError _err -> raise (Error "session") (* todo *))


let make_bind t session =
  make_iq_request t ~jid_to:(domain t.myjid)
    (IQSet (make_element (ns_xmpp_bind, "bind") []
              [make_simple_cdata (ns_xmpp_bind, "resource") t.myjid.resource]))
    (fun ev _jid_from _jid_to _lang () ->
       match ev with
         | IQResult (Some el) ->
             let myjid = get_cdata
               (get_subelement (ns_xmpp_bind, "jid") el) in
               t.myjid <- jid_of_string myjid;
               make_session t session
         | IQResult None
         | IQError _ ->
             raise (Error "bind")
    )
          
exception AuthError of string
exception AuthFailure of string

let sasl_digest t password nextstep =
  let rec step1 t _qname _attrs els =
    unregister_stanza_handler t (ns_xmpp_sasl, "challenge");
	  let ch_text = collect_cdata els in
	  let resp = Sasl.sasl_digest_response ch_text
      t.myjid.lnode t.myjid.domain password in
      register_stanza_handler t (ns_xmpp_sasl, "") step2;
      t.socket.send (Xmlstream.stanza_serialize t.p
                       (make_element (ns_xmpp_sasl, "response") []
                          [Xmlcdata resp]))
  and step2 t qname _attrs els =
    unregister_stanza_handler t (ns_xmpp_sasl, "");
    match get_name qname with
      | "failure" ->
          let p = get_first_element els in
	          raise (AuthFailure (get_name (get_qname p)))
      | "challenge" ->
		      Sasl.sasl_digest_rspauth (collect_cdata els);
          register_stanza_handler t (ns_xmpp_sasl, "") step3;
          t.socket.send (Xmlstream.stanza_serialize t.p
                           (make_element (ns_xmpp_sasl, "response") [] []))
      | "success" ->
		      nextstep t
      | _ ->
          raise (AuthError "Invalid XMPProtocol")
  and step3 t qname _attrs els =
    unregister_stanza_handler t (ns_xmpp_sasl, "");
    match get_name qname with
      | "success" ->
          nextstep t
      | "failure" ->
          let p = get_first_element els in
	          raise (AuthFailure (get_name (get_qname p)))
			| _ ->
				  raise (AuthError "Invalid XMPProtocol")
  in
    register_stanza_handler t (ns_xmpp_sasl, "challenge") step1;
    t.socket.send (Xmlstream.stanza_serialize t.p
                     (make_element (ns_xmpp_sasl, "auth")
                        [make_attr "mechanism" "DIGEST-MD5"] []))
      
let sasl_plain t password nextstep =
  let sasl_data = 
    Cryptokit.transform_string (Cryptokit.Base64.encode_compact  ())
	    (Printf.sprintf "%s\x00%s\x00%s"
	       (t.myjid.node ^ "@" ^ t.myjid.domain)
         t.myjid.node password)  ^ "==" in
    register_stanza_handler t (ns_xmpp_sasl, "")
      (fun t qname _attrs els ->
         unregister_stanza_handler t (ns_xmpp_sasl, "");
         match get_name qname with
           | "failure" ->
               let p = get_first_element els in
	               raise (AuthFailure (get_name (get_qname p)))
           | "success" ->
		           nextstep t
           | _ ->
               raise (AuthError "Invalid XMPProtocol")
      );        
    t.socket.send (Xmlstream.stanza_serialize t.p
                     (make_element (ns_xmpp_sasl, "auth")
                        [make_attr "mechanism" "PLAIN"] [Xmlcdata sasl_data]))


let sasl_auth t features password  session =
  let mechanisms = get_element (ns_xmpp_sasl, "mechanisms") features in
  let mels = get_subelements (ns_xmpp_sasl, "mechanism") mechanisms in
  let m = List.map get_cdata mels in
  let nextstep t =
    Xmlstream.reset t.p;
    register_stanza_handler t (ns_streams, "features")
      (fun t _qname _attrs els ->
         unregister_stanza_handler t (ns_streams, "features");
         register_stanza_handler t (ns_client, "iq") process_iq;
         if mem_qname (ns_xmpp_bind, "bind") els then
           make_bind t session
         else
           raise (Error "no bind")
      );
    t.socket.send (Xmlstream.stream_header t.p (ns_streams, "stream")
                     [make_attr "to" t.myjid.domain; make_attr "version" "1.0"])
  in    
    if List.mem "DIGEST-MD5" m then
      sasl_digest t password nextstep
    else if List.mem "PLAIN" m then
      sasl_plain t password nextstep
    else
      raise (AuthError "no known SASL method")
        
let create data socket ?(lang="") myjid =
  let p = Xmlstream.create [ns_streams; ns_client] in
  let () = Xmlstream.bind_prefix p "stream" ns_streams in
    {
      socket = socket;
      sid = 1;
      xmllang = lang;
      iq_response = IDCallback.empty;
      iq_request = IQRequestCallback.empty;
      stanza_handlers = StanzaHandler.empty;
      myjid = myjid;
      p = p;
      data = data
    }

let starttls t sasl_step =
  let nextstep t =
    Xmlstream.reset t.p;
    register_stanza_handler t (ns_streams, "features") sasl_step;
    t.socket.send (Xmlstream.stream_header t.p (ns_streams, "stream")
                     ((make_attr "to" t.myjid.domain) ::
                        (make_attr "version" "1.0") ::
                        (if t.xmllang = "" then [] else
                           [make_attr ~ns:ns_xml "lang" t.xmllang])))
  in
    register_stanza_handler t (ns_xmpp_tls, "")
      (fun t qname _attrs els ->
         unregister_stanza_handler t (ns_xmpp_tls, "");
         if qname = (ns_xmpp_tls, "proceed") then (
           Transport.switch t.socket;
           nextstep t
         )
         else if qname = (ns_xmpp_tls, "failure") then
           let p = get_first_element els in
	           raise (AuthFailure (get_name (get_qname p)))
         else
           raise (Error "starttls")
      );
    t.socket.send (Xmlstream.stanza_serialize t.p
                     (make_element (ns_xmpp_tls, "starttls") [] []))

let open_stream t ?(use_tls=false) password session =
  let sasl_step =
    (fun t _qname _attrs els ->
       unregister_stanza_handler t (ns_streams, "features");
       sasl_auth t els password session
    ) in
    register_stanza_handler t (ns_streams, "features")
      (fun t _qname _attrs els ->
         unregister_stanza_handler t (ns_streams, "features");
         let tls =
           try Some (get_element (ns_xmpp_tls, "starttls") els)
           with Not_found -> None in
         let use_tls =
           match tls with
             | None -> false
             | Some el ->
                 if mem_child (ns_xmpp_tls, "required") el && not can_tls then
                   raise (Error "TLS is required")
                 else use_tls && can_tls
         in
           if use_tls then
             starttls t sasl_step
           else
             sasl_auth t els password session
      );
    t.socket.send (stream_header t.p (ns_streams, "stream")
                     ((make_attr "to" t.myjid.domain) ::
                        (make_attr "version" "1.0") ::
                        (if t.xmllang = "" then [] else
                           [make_attr ~ns:ns_xml "lang" t.xmllang])))
      
let close_stream t =
  t.socket.send (Xmlstream.stream_end t.p (ns_streams, "stream"))

let stanza t qname attrs els =
  if qname = (ns_streams, "error") then
    raise (StreamError (StreamError.parse_error els))
  else
    let callback =
      try Some (StanzaHandler.find qname t.stanza_handlers)
      with Not_found ->
        let namespace = get_namespace qname in
          try Some (StanzaHandler.find (namespace, "") t.stanza_handlers)
          with Not_found -> None in
      match callback with
        | None -> ()
        | Some f ->
            try f t qname attrs els
            with
              | BadRequest ->
                  send_error_reply t ERR_BAD_REQUEST qname attrs els
              | MalformedStanza -> ()

let stream_start qname attrs =
  if qname = (ns_streams, "stream") &&
    get_attr_value "version" attrs  = "1.0" then
      ()
  else
    raise (Error "bad stream header")

let stream_end () =
    raise End_of_file

let parse t =
  let str = t.socket.read () in
    Xmlstream.add_buffer t.p str;
    Xmlstream.parse t.p stream_start (stanza t) stream_end
      
