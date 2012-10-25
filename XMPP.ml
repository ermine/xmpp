(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open Xml
open Xmlstream
open StanzaError
open JID

type id = string

module type IDCALLBACK =
sig
  type 'a t
  val empty : 'a t
  val add : id -> 'a -> 'a t -> 'a t
  val find : id -> 'a t -> 'a
  val remove : id -> 'a t -> 'a t
end

module type S =
sig
  exception Error of string
  exception StreamError of StreamError.t
  exception MalformedStanza
  exception BadRequest

  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  type iq_request =
    | IQSet of element
    | IQGet of element
        
  type iq_response =
    | IQResult of element option
    | IQError of StanzaError.t
        
  type iq =
    | IQRequest of iq_request
    | IQResponse of iq_response


  type 'a session_data

  val get_myjid : 'a session_data -> JID.t

  val make_iq_request :
    'a session_data ->
    ?jid_from:JID.t ->
    ?jid_to:JID.t ->
    ?lang:Xml.cdata ->
    iq_request ->
    (iq_response ->
     string option -> string option -> string option -> unit -> unit t) ->
    unit t

  type 'a stanza = {
    id : id option;
    jid_from : JID.t option;
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

  val send_message :
    'a session_data ->
    ?id:Xml.cdata ->
    ?jid_from:JID.t ->
    ?jid_to:JID.t ->
    ?kind:message_type ->
    ?lang:Xml.cdata ->
    ?body:Xml.cdata ->
    ?subject:Xml.cdata ->
    ?thread:Xml.cdata -> ?x:Xml.element list -> unit -> unit t

  type presence_type =
      Probe
    | Subscribe
    | Subscribed
    | Unsubscribe
    | Unsubscribed
    | Unavailable
  val string_of_presence_type : presence_type -> string
  type presence_show = ShowChat | ShowAway | ShowDND | ShowXA
  val string_of_show : presence_show -> string
  type presence_content = {
    presence_type : presence_type option;
    show : presence_show option;
    status : string option;
    priority : int option;
  }
      
  type presence_stanza = presence_content stanza
  val parse_presence :
    callback:('a session_data -> presence_content stanza -> unit t) ->
    callback_error:('a session_data ->
                    ?id:id ->
                    ?jid_from:JID.t ->
                    ?jid_to:id -> ?lang:id -> StanzaError.t -> unit t) ->
    'a session_data ->
    Xml.attribute list -> Xml.element list -> unit t
  val send_presence :
    'a session_data ->
    ?id:Xml.cdata ->
    ?jid_from:JID.t ->
    ?jid_to:JID.t ->
    ?kind:presence_type ->
    ?lang:Xml.cdata ->
    ?show:presence_show ->
    ?status:Xml.cdata ->
    ?priority:int -> ?x:Xml.element list -> unit -> unit t
end

module Make (M : MONAD) (XmlParser : Xmlstream.S) (IDCallback : IDCALLBACK) =
struct
  include M
  module X = XmlParser (M)

  exception Error of string
  exception StreamError of StreamError.t
  exception MalformedStanza
  exception BadRequest
    
  module NS =
  struct
    type t = Xml.namespace
    let compare = Pervasives.compare
  end
  module IQRequestCallback = Map.Make(NS)

  module Qname =
  struct
    type t = Xml.qname
    let compare = Pervasives.compare
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


  module type Socket =
  sig
    type t
    val socket : t
    val read : t -> string -> int -> int -> int M.t
    val write : t -> string -> unit M.t
    val close : t -> unit M.t
  end
      
  type 'a session_data = {
    mutable socket : (module Socket);
    p : X.p;
    mutable sid : int;
    mutable iq_response :
      (iq_response -> string option -> string option -> string option -> unit ->
       unit t) IDCallback.t;
    mutable iq_request :
      (iq_request -> string option -> string option -> string option -> unit ->
       iq_response t) IQRequestCallback.t;
    mutable stanza_handlers :
      ('a session_data -> Xml.attribute list -> Xml.element list ->
       unit t) StanzaHandler.t;
    mutable myjid : JID.t;
    ser: Xml.Serialization.t;
    user_data : 'a
  }
      
  let string_of_option opt = match opt with None -> "" | Some v -> v
  let maybe f = function None -> None | Some v -> Some (f v)

  let get_myjid session_data = session_data.myjid

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
      
  let send session_data v =
    let module S = (val session_data.socket : Socket) in
      S.write S.socket v
          
  let make_iq_request session_data ?jid_from ?jid_to ?lang request callback =
    session_data.sid <- session_data.sid + 1;
    let id =
      string_of_int session_data.sid ^ ":" ^ string_of_int (Random.int 1000) in
    let kind, el =
      match request with
        | IQSet el -> "set", el
        | IQGet el -> "get", el
    in
    let jid_to = maybe string_of_jid jid_to in
    let jid_from = maybe string_of_jid jid_from in
    let attrs = make_stanza_attrs ~id ~kind ?jid_from ?jid_to ?lang () in
      session_data.iq_response <-
        IDCallback.add id callback session_data.iq_response;
      send session_data
        (Xmlstream.stanza_serialize session_data.ser
           (make_element (ns_client, "iq") attrs [el]))
        
  type 'a stanza = {
    id : id option;
    jid_from : JID.t option;
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
      
  let parse_message ~callback ~callback_error session_data attrs els =
    let id, jid_from, jid_to, kind, lang = parse_stanza_attrs attrs in
    let jid_from = maybe JID.of_string jid_from in
      if kind = Some "error" then
        let err = StanzaError.parse_error
          (get_element (ns_client, "error") els) in
          callback_error session_data ?id ?jid_from ?jid_to ?lang err
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
          callback session_data message_stanza
            
  let send_message session_data ?id ?jid_from ?jid_to ?kind ?lang
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
      send session_data
        (Xmlstream.stanza_serialize session_data.ser
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

  let parse_presence ~callback ~callback_error session_data attrs els =
    let id, jid_from, jid_to, kind, lang = parse_stanza_attrs attrs in
    let jid_from = maybe JID.of_string jid_from in
      if kind = Some "error" then
        let err = StanzaError.parse_error
          (get_element (ns_client, "error") els) in
          callback_error session_data ?id ?jid_from ?jid_to ?lang err
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
          callback session_data presence_stanza
            
  let send_presence session_data ?id ?jid_from ?jid_to ?kind ?lang
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
      send session_data
        (Xmlstream.stanza_serialize session_data.ser
           (make_element (ns_client, "presence") attrs els))
        
  let send_error_reply session_data condition ?text ?lang qname attrs els =
    let ns = get_namespace qname in
    let error = make_error ~ns ?text ?lang condition in
    let newattrs = make_stanza_attrs_reply ~kind:"error" attrs in
      send session_data
        (Xmlstream.stanza_serialize session_data.ser
           (make_element qname newattrs (error::els)))
        
  let register_iq_request_handler session_data namespace handler =
    session_data.iq_request <-
      IQRequestCallback.add namespace handler session_data.iq_request
      
  let unregister_iq_request_handler session_data namespace =
    session_data.iq_request <-
      IQRequestCallback.remove namespace session_data.iq_request
      
  let register_stanza_handler session_data qname handler =
    session_data.stanza_handlers <-
      StanzaHandler.add qname handler session_data.stanza_handlers
      
  let unregister_stanza_handler session_data qname =
    session_data.stanza_handlers <-
      StanzaHandler.remove qname session_data.stanza_handlers
      
  let process_iq session_data attrs els =
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
            try Some (IQRequestCallback.find namespace session_data.iq_request)
            with Not_found -> None in
            (match proc with
              | None ->
                return (IQError (create_error ERR_FEATURE_NOT_IMPLEMENTED))
              | Some f ->
                f ev jid_from jid_to lang ()
            ) >>= fun response ->
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
                send session_data (Xmlstream.stanza_serialize
                                              session_data.ser xml)
        )
          
        | IQResponse ev -> (
          match id with
            | None ->
              raise BadRequest
            | Some id -> 
              try
                let f = IDCallback.find id session_data.iq_response in
                  session_data.iq_response <- IDCallback.remove id
                    session_data.iq_response;
                  f ev jid_from jid_to lang ()
              with Not_found -> return ()
        )
          
  let make_session session_data session_handler =
    make_iq_request session_data ~jid_from:session_data.myjid
      (IQSet (make_element (ns_xmpp_session, "session") [] []))
      (fun ev _jid_from _jid_to _lang () ->
        match ev with
          | IQResult _ -> session_handler session_data
          | IQError _err -> fail (Error "session") (* todo *))
      
  let make_bind session_data session_handler =
    make_iq_request session_data ~jid_to:(domain session_data.myjid)
      (IQSet (make_element (ns_xmpp_bind, "bind") []
                [make_simple_cdata (ns_xmpp_bind, "resource")
                    session_data.myjid.resource]))
      (fun ev _jid_from _jid_to _lang () ->
        match ev with
          | IQResult (Some el) ->
            let myjid = get_cdata
              (get_subelement (ns_xmpp_bind, "jid") el) in
              session_data.myjid <- JID.of_string myjid;
              make_session session_data session_handler
          | IQResult None
          | IQError _ ->
            raise (Error "bind")
      )
      
  exception AuthError of string
  exception AuthFailure of string
      
  let sasl_digest session_data password nextstep =
    let rec step1 session_data _attrs els =
      unregister_stanza_handler session_data (ns_xmpp_sasl, "challenge");
	    let ch_text = collect_cdata els in
	    let resp = Sasl.sasl_digest_response ch_text
        session_data.myjid.lnode session_data.myjid.domain password in
        register_stanza_handler
          session_data (ns_xmpp_sasl, "challenge") step2_challenge;
        register_stanza_handler
          session_data (ns_xmpp_sasl, "success") step2_success;
        register_stanza_handler
          session_data (ns_xmpp_sasl, "failure") step2_failure;
        send session_data
          (Xmlstream.stanza_serialize session_data.ser
             (make_element (ns_xmpp_sasl, "response") [] [Xmlcdata resp]))
    and step2_challenge session_data _attrs els =
      unregister_stanza_handler session_data (ns_xmpp_sasl, "challenge");
		  Sasl.sasl_digest_rspauth (collect_cdata els);
      send session_data
        (Xmlstream.stanza_serialize session_data.ser
           (make_element (ns_xmpp_sasl, "response") [] []))
    and step2_failure session_data _attrs els =
      unregister_stanza_handler session_data (ns_xmpp_sasl, "challenge");
      unregister_stanza_handler session_data (ns_xmpp_sasl, "failure");
      unregister_stanza_handler session_data (ns_xmpp_sasl, "success");
      let p = get_first_element els in
	      fail (AuthFailure (get_name (get_qname p)))
    and step2_success session_data _attrs els =
      unregister_stanza_handler session_data (ns_xmpp_sasl, "challenge");
      unregister_stanza_handler session_data (ns_xmpp_sasl, "failure");
      unregister_stanza_handler session_data (ns_xmpp_sasl, "success");
		  nextstep session_data

    in
      register_stanza_handler session_data (ns_xmpp_sasl, "challenge") step1;
      send session_data
        (Xmlstream.stanza_serialize session_data.ser
           (make_element (ns_xmpp_sasl, "auth")
              [make_attr "mechanism" "DIGEST-MD5"] []))
        
  let sasl_plain session_data password nextstep =
    let sasl_data = 
      Cryptokit.transform_string (Cryptokit.Base64.encode_compact  ())
	      (Printf.sprintf "%s\x00%s\x00%s"
	         (session_data.myjid.node ^ "@" ^ session_data.myjid.domain)
           session_data.myjid.node password)  ^ "==" in
      register_stanza_handler session_data (ns_xmpp_sasl, "failure")
        (fun session_data _attrs els ->
          unregister_stanza_handler session_data (ns_xmpp_sasl, "success");
          unregister_stanza_handler session_data (ns_xmpp_sasl, "failure");
          let p = get_first_element els in
	          raise (AuthFailure (get_name (get_qname p)))
        );
      register_stanza_handler session_data (ns_xmpp_sasl, "success")
        (fun session_data _attrs els ->
          unregister_stanza_handler session_data (ns_xmpp_sasl, "failure");
          unregister_stanza_handler session_data (ns_xmpp_sasl, "success");
		      nextstep session_data
        );
      send session_data
        (Xmlstream.stanza_serialize session_data.ser
           (make_element (ns_xmpp_sasl, "auth")
              [make_attr "mechanism" "PLAIN"] [Xmlcdata sasl_data]))
        
        
  let sasl_auth session_data features lang password  session_handler =
    let mechanisms = get_element (ns_xmpp_sasl, "mechanisms") features in
    let mels = get_subelements (ns_xmpp_sasl, "mechanism") mechanisms in
    let m = List.map get_cdata mels in
    let nextstep session_data =
      register_stanza_handler session_data (ns_streams, "features")
        (fun session_data _attrs els ->
          unregister_stanza_handler session_data (ns_streams, "features");
          register_stanza_handler session_data (ns_client, "iq") process_iq;
          if mem_qname (ns_xmpp_bind, "bind") els then
            make_bind session_data session_handler
          else
            raise (Error "no bind")
        );
      X.reset session_data.p None;
      send session_data
        (Xmlstream.stream_header session_data.ser
           (ns_streams, "stream")
           (make_attr "to" session_data.myjid.domain ::
              make_attr "version" "1.0" ::
              (match lang with
                | None -> []
                | Some v -> [make_attr ~ns:ns_xml "lang" v])))
    in    
      if List.mem "DIGEST-MD5" m then
        sasl_digest session_data password nextstep
      else if List.mem "PLAIN" m then
        sasl_plain session_data password nextstep
      else
        fail (AuthError "no known SASL method")
      
  let starttls session_data tls_module lang password session_handler =
    register_stanza_handler session_data (ns_xmpp_tls, "proceed")
      (fun session_data _attrs els ->
        unregister_stanza_handler session_data (ns_xmpp_tls, "proceed");
        unregister_stanza_handler session_data (ns_xmpp_tls, "failure");
        register_stanza_handler session_data (ns_streams, "features")
          (fun session_data _attrs els ->
            sasl_auth session_data els lang password session_handler);
        tls_module session_data >>= fun () ->
        send session_data
            (Xmlstream.stream_header session_data.ser
               (ns_streams, "stream")
               (make_attr "to" session_data.myjid.domain ::
                  make_attr "version" "1.0" ::
                  (match lang with
                    | None -> []
                    | Some v -> [make_attr ~ns:ns_xml "lang" v])))
      );
    register_stanza_handler session_data (ns_xmpp_tls, "failure")
      (fun session_data _attrs els ->
        unregister_stanza_handler session_data (ns_xmpp_tls, "proceed");
        unregister_stanza_handler session_data (ns_xmpp_tls, "failure");
        let p = get_first_element els in
	        raise (AuthFailure (get_name (get_qname p)))
      );
    send session_data
      (Xmlstream.stanza_serialize session_data.ser
         (make_element (ns_xmpp_tls, "starttls") [] []))

  let start_stream session_data ?tls ?compress lang password session_handler =
    register_stanza_handler session_data (ns_streams, "features")
      (fun session_data _attrs els ->
        unregister_stanza_handler session_data (ns_streams, "features");
        let tls_el =
          try Some (get_element (ns_xmpp_tls, "starttls") els)
          with Not_found -> None in
          match tls_el with
            | None -> sasl_auth session_data els lang password session_handler
            | Some el ->
              if mem_child (ns_xmpp_tls, "required") el then
                match tls with
                  | None -> raise (Error "TLS is required")
                  | Some m ->
                    starttls session_data m lang password session_handler
              else
                match tls with
                  | None ->
                    sasl_auth session_data els lang password session_handler
                  | Some m ->
                    starttls session_data m lang password session_handler
      );
    return ()
      
  let close_stream session_data =
    send session_data
      (Xmlstream.stream_end session_data.ser (ns_streams, "stream"))
      
  let stream_stanza session_data (qname, attrs, els) =
    if qname = (ns_streams, "error") then
      raise (StreamError (StreamError.parse_error els))
    else
      let callback =
        try Some (StanzaHandler.find qname session_data.stanza_handlers)
        with Not_found -> None in
        match callback with
          | None -> return ()
          | Some f ->
            try f session_data attrs els
            with
              | BadRequest ->
                send_error_reply session_data ERR_BAD_REQUEST qname attrs els
              | MalformedStanza -> return ()
                
  let stream_start qname attrs =
    if qname = (ns_streams, "stream") &&
      get_attr_value "version" attrs  = "1.0" then
      return ()
    else
      fail (Error "bad stream header")
        
  let stream_end session_data () = return ()

  let create_session_data plain_socket myjid user_data =
    let ser = Xml.Serialization.create [ns_streams; ns_client] in
    let () = Xmlstream.bind_prefix ser "stream" ns_streams in
    let read buf start len =
      let module S = (val plain_socket : Socket) in
        S.read S.socket buf start len
    in
      {
        socket = plain_socket;
        p = X.create read;
        sid = 1;
        iq_response = IDCallback.empty;
        iq_request = IQRequestCallback.empty;
        stanza_handlers = StanzaHandler.empty;
        myjid = myjid;
        ser = ser;
        user_data = user_data
      }
      
  let open_stream session_data ?tls_socket ?lang password session_handler =
    send session_data
      (Xmlstream.stream_header session_data.ser
         (ns_streams, "stream")
         (make_attr "to" session_data.myjid.domain ::
            make_attr "version" "1.0" ::
            (match lang with
              | None -> []
              | Some v -> [make_attr ~ns:ns_xml "lang" v]))) >>= fun () ->
    start_stream session_data
      ?tls:(match tls_socket with
        | None -> None
        | Some socket -> Some (fun session_data ->
          socket () >>= fun socket ->
          session_data.socket <- socket;
          let read buf start len =
            let module S = (val socket : Socket) in
              S.read S.socket buf start len
          in
            X.reset session_data.p (Some read);
            return ()
        ))
      lang password session_handler

  let setup_session
      ~myjid
      ~user_data
      ~(plain_socket : (module Socket))
      ?(tls_socket : (unit -> (module Socket) M.t) option)
      ?lang
      ~password session_handler =
    let session_data = create_session_data plain_socket myjid user_data in
    open_stream session_data ?tls_socket ?lang password session_handler
    >>= fun () -> return session_data
    
  let parse session_data =
    X.parse session_data.p
      stream_start (stream_stanza session_data) (stream_end session_data)
end
