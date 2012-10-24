open XMPP
open StanzaError

module UnitMonad =
struct
  type 'a t = 'a
  let return x = x
  let (>>=) v f = f v
  let fail exn = raise exn
  let catch f1 f2 = try f1 () with exn -> f2 exn
end    

module SimpleTransport =
struct
  type 'a z = 'a UnitMonad.t
  type fd = unit
      
  type socket = {
    inc : in_channel;
    outc : out_channel;
  }

  let can_tls = false
  let can_compress = false

  let open_connection sockaddr =
    let inc, outc = Unix.open_connection sockaddr in
      {inc;
       outc
      }
  let read s buf start len =
    let size = input s.inc buf start len in
      if size > 0 then
        print_string "IN: "; print_endline (String.sub buf start size);
      size

  let write s str =
    print_string "OUT: ";
    print_endline str;
    flush stdout;
    output_string s.outc str;
    flush s.outc

  let close s =
    print_endline "closing socket";
    close_out s.outc

end

module ID =
struct
  type t = id
  let compare = Pervasives.compare
end
module IDCallback =
struct
  module T = Treap.Map(ID)
  type 'a t = 'a T.t
  let empty = T.empty
  let add key value t = T.add t key value 1
  let remove key t = T.delete t key
  let find key t = fst (T.find t key)
end

module XMPPClient = Make (UnitMonad) (Xmlstream.XmlStreamIE) (IDCallback)

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
  print_endline ("message error: " ^ error.err_text)
    
let presence_callback t stanza =
  match stanza.content.presence_type with
    | None -> print_endline "available"
    | Some _ -> print_endline "something"
  
let presence_error t ?id ?jid_from ?jid_to ?lang error =
  print_endline ("presence error: " ^ error.err_text)
    
let session t =
  register_iq_request_handler t Version.ns_version
    (fun ev _jid_from _jid_to _lang () ->
       match ev with
         | IQGet _el ->
             let el = Version.encode {Version.name = "xmpptest";
                                      Version.version = "2.0";
                                      Version.os = Sys.os_type} in
               IQResult (Some el)
         | IQSet _el ->
             raise BadRequest
    );
  register_stanza_handler t (ns_client, "message")
    (parse_message ~callback:message_callback ~callback_error:message_error);
  register_stanza_handler t (ns_client, "presence")
    (parse_presence ~callback:presence_callback ~callback_error:presence_error)

open Xml
open JID

let open_stream
    ~myjid
    ~user_data
    ~(plain_socket : (module Socket))
    ?(tls_socket : (unit -> (module XMPPClient.Socket) UnitMonad.t) option)
    ?lang
    ~password session_handler =
  let session_data = create_session_data plain_socket myjid user_data in
    send session_data
      (Xmlstream.stream_header session_data.ser
         (ns_streams, "stream")
         (make_attr "to" session_data.XMPPClient.myjid.domain ::
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
    lang password session_handler >>=
    fun () ->
    let rec loop () =
      X.parse session_data.p
        stream_start (stream_stanza session_data) (stream_end session_data) >>=
        loop
    in
      catch loop (function exn ->
        let module S = (val session_data.socket : Socket) in
          S.close S.socket;
          fail exn
      )

let () =
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
  let socket_data = SimpleTransport.open_connection sockaddr in

  let module Socket_module =
      struct
        type t = SimpleTransport.socket
        let socket = socket_data
        include SimpleTransport
      end
  in
    XMPPClient.setup_session
      ~user_data:()
      ~myjid
      ~plain_socket:(module Socket_module : XMPPClient.Socket)
      ~password session >>= fun session_data ->
    let rec loop () =
      X.parse session_data.p
        stream_start (stream_stanza session_data) (stream_end session_data) >>=
        loop
    in
      catch loop (function
        | End_of_file ->
          let module S = (val session_data.socket : Socket) in
            S.close S.socket
        | exn ->
          fail exn
      )
