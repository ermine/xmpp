open Transport
open XMPP
open StanzaError

let message_callback t ?id ?jid_from ?jid_to ?type_ ?lang
    ?body ?subject ?thread x () =
  (match body with
     | None -> ()
     | Some v -> print_endline v);
  send_message t ?jid_to:jid_from ?id ?type_ ?lang ?body ()

let message_error t ?id ?jid_from ?jid_to ?lang error =
  print_endline ("message error: " ^ error.err_text)
    
let presence_callback t ?id ?jid_from ?jid_to ?type_ ?lang
    ?show ?status ?priority x () =
  match type_ with
    | None -> print_endline "available"
    | Some _ -> print_endline "something"
  
let presence_error t ?id ?jid_from ?jid_to ?lang error =
  print_endline ("presence error: " ^ error.err_text)
    
let session t =
  XMPP.register_iq_request_handler t Xep_version.ns_version
    (fun ev _jid_from _jid_to _lang () ->
       match ev with
         | IQGet _el ->
             let el = Xep_version.encode {Xep_version.name = "xmpptest";
                                          Xep_version.version = "2.0";
                                          Xep_version.os = Sys.os_type} in
               IQResult (Some el)
         | IQSet _el ->
             raise BadRequest
    );
  XMPP.register_stanza_handler t (ns_client, "message")
    (parse_message ~callback:message_callback ~callback_error:message_error);
  XMPP.register_stanza_handler t (ns_client, "presence")
    (parse_presence ~callback:presence_callback ~callback_error:presence_error)

let _ =
  let server = "jabber.ru"
  and username = "zzz"
  and password = "****"
  and resource = "xmpp2.0"
  and port = 5222 in
  let myjid = Jid.make_jid username server resource in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let socket = {
    fd = s;
    send = send s;
    read = read s
  } in
  let xmpp = XMPP.create socket myjid in
    connect s server port;
    XMPP.open_stream xmpp ~use_tls:true password session;
    let rec loop () =
      XMPP.parse xmpp;
      loop ()
    in
      loop ()
 
