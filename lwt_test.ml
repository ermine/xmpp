open StanzaError

module LWTTransport =
struct
  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.(>>=)
  let catch = Lwt.catch
  let fail = Lwt.fail

  open Lwt_ssl

  type strm = {
    buf : string;
    mutable i : int;
    mutable len : int
  }
  type socket = {
    fd : Lwt_unix.file_descr;
    mutable socket : Lwt_ssl.socket;
    strm : strm;
  }

  let can_tls = true
  let can_compress = true
    
  let open_connection sockaddr =
    let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt_unix.connect fd sockaddr >>= fun () ->
    let socket = Lwt_ssl.plain fd in
      return {fd = fd;
              socket;
              strm = { buf = String.create 8192; i = 0; len = 0};
             }

  let get s =
    if s.strm.i < s.strm.len then
      let c = s.strm.buf.[s.strm.i] in
        s.strm.i <- s.strm.i + 1;
        return (Some c)
    else
      Lwt_ssl.read s.socket s.strm.buf 0 8192 >>=
        (fun size ->
          if size = 0 then
            return None
          else (
            print_string "IN: "; print_endline (String.sub s.strm.buf 0 size);
            s.strm.len <- size;
            s.strm.i <- 1;
            return (Some s.strm.buf.[0])
          )
        )

  let send s str =
    print_string "OUT: ";
    print_endline str;
    let len = String.length str in
    let rec aux_send start =
        Lwt_ssl.write s.socket str start (len - start) >>= fun sent ->
    if sent = 0 then
      return ()
    else
      aux_send (start + sent)
    in
      aux_send 0

  let switch_tls s =
    Ssl.init ();
    let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
    let fd = s.fd in
    Lwt_ssl.ssl_connect fd ctx >>= fun newsocket ->
    s.socket <- newsocket;
      return ()
        
  let close s =
    Lwt_ssl.close s.socket
end

module ID =
struct
  type t = string
  let compare = Pervasives.compare
end
module IDCallback = Map.Make(ID)

module XMPPClient = XMPP.Make (LWTTransport) (IDCallback)

open XMPPClient

module Version = XEP_version.Make (XMPPClient)

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
  print_endline ("message error: " ^ error.err_text);
  return ()
    
let presence_callback t stanza =
  (match stanza.content.presence_type with
    | None -> print_endline "available"
    | Some _ -> print_endline "something"
  ); return ()
  
let presence_error t ?id ?jid_from ?jid_to ?lang error =
  print_endline ("presence error: " ^ error.err_text);
  return ()
    
let session t =
  register_iq_request_handler t Version.ns_version
    (fun ev _jid_from _jid_to _lang () ->
      match ev with
        | IQGet _el ->
          let el = Version.encode {Version.name = "xmpptest";
                                   Version.version = "2.0";
                                   Version.os = Sys.os_type} in
            return (IQResult (Some el))
        | IQSet _el ->
          fail BadRequest
    );
  register_stanza_handler t (ns_client, "message")
    (parse_message ~callback:message_callback ~callback_error:message_error);
  register_stanza_handler t (ns_client, "presence")
    (parse_presence ~callback:presence_callback ~callback_error:presence_error);
  return ()
  
let _ =
  let server = Sys.argv.(1)
  and username = Sys.argv.(2)
  and password = Sys.argv.(3)
  and resource = "xmpp3.0"
  and port =
    if Array.length Sys.argv < 5 then 5222 else int_of_string Sys.argv.(4) in

  let myjid = JID.make_jid username server resource in
  let inet_addr =
    try Unix.inet_addr_of_string server
    with Failure("inet_addr_of_string") ->
      (Unix.gethostbyname server).Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  let data = () in
    Lwt_main.run (
      LWTTransport.open_connection sockaddr >>= fun socket_data ->
      create data socket_data myjid >>= fun xmpp ->
      XMPPClient.open_stream xmpp ~use_tls:true password session
    )
  
    
