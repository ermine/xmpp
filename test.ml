open Transport
open XMPP
open StanzaError

let message_callback t stanza =
  (match stanza.content.body with
     | None -> ()
     | Some v -> print_endline v);
  send_message t ?jid_to:stanza.jid_from
    ?id:stanza.id
    ?kind:stanza.content.message_type
    ?lang:stanza.lang
    ?body:stanza.content.body ()

let message_error t ?id ?jid_from ?jid_to ?lang error =
  print_endline ("message error: " ^ error.err_text)
    
let presence_callback t stanza =
  match stanza.content.presence_type with
    | None -> print_endline "available"
    | Some _ -> print_endline "something"
  
let presence_error t ?id ?jid_from ?jid_to ?lang error =
  print_endline ("presence error: " ^ error.err_text)
    
let session t =
  XMPP.register_iq_request_handler t XEP_version.ns_version
    (fun ev _jid_from _jid_to _lang () ->
       match ev with
         | IQGet _el ->
             let el = XEP_version.encode {XEP_version.name = "xmpptest";
                                          XEP_version.version = "2.0";
                                          XEP_version.os = Sys.os_type} in
               IQResult (Some el)
         | IQSet _el ->
             raise BadRequest
    );
  XMPP.register_stanza_handler t (ns_client, "message")
    (parse_message ~callback:message_callback ~callback_error:message_error);
  XMPP.register_stanza_handler t (ns_client, "presence")
    (parse_presence ~callback:presence_callback ~callback_error:presence_error)

let _ =
  let server = Sys.argv.(1)
  and username = Sys.argv.(2)
  and password = Sys.argv.(3)
  and resource = "xmpp2.0"
  and port =
    if Array.length Sys.argv < 5 then 5222 else int_of_string Sys.argv.(4) in

  let myjid = JID.make_jid username server resource in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let socket = {
    fd = s;
    send = send s;
    read = read s
  } in
  let data = () in
  let xmpp = XMPP.create data socket myjid in
    connect s server port;
    XMPP.open_stream xmpp ~use_tls:true password session;
    let rec loop () =
      XMPP.parse xmpp;
      loop ()
    in
      loop ()
 
