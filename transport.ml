(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

module type TRANSPORT =
sig
  type 'a t
  val return : 'a -> 'a t
  val fail : exn -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  type socket
  val can_tls : bool
  val can_compress : bool
  val open_connection : Unix.sockaddr -> socket t
  val get : socket -> char option t
  val send : socket -> string -> unit t
  val switch_tls : socket -> unit t
  val close : socket -> unit t
end

open Unix

module SimpleTransport =
struct
  type 'a t = 'a
  type socket = {
    inc : in_channel;
    outc : out_channel;
    strm : char Stream.t;
  }
  let open_connection sockaddr =
    let inc, outc = Unix.open_connection sockaddr in
      {inc = inc; strm = Stream.of_channel inc; outc = outc}
  let peek s = Stream.peek s.strm
  let junk s = Stream.junk s.strm
  let send s = output_string s.outc
  let close s =
    close_in s.inc;
    close_out s.outc
end
  
(*
  
type t = {
  fd : file_descr;
  mutable read: unit -> string;
  mutable send: string -> unit
}

let connect s host port =
  let inet_addr =
    try Unix.inet_addr_of_string host
    with Failure("inet_addr_of_string") ->
      (Unix.gethostbyname host).Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
    Unix.connect s sockaddr
    
let read s () =
  let string = String.create 8193 in
  let size = Unix.read s string 0 8192 in
    if size = 0 then (
      close s;
      ""
    ) else (
      String.sub string 0 size
    )
          
let send s string =
  let rec aux_send off =
    let size = Unix.write s string off (String.length string - off) in
      if off + size < String.length string then
        aux_send (off + size)
      else
        ()
  in
    aux_send 0


open Mltls

type machine = {
  ctx: tls_SSL_CTX;
  ssl: tls_SSL
}

let new_machine fd =
  tls_init ();
  let ctx = tls_SSL_CTX_new SSLv23_method in
  let ssl = tls_SSL_new ctx in
  let n = tls_SSL_set_fd ssl fd in
  let () =
    if n <> 1 then
      (*
      let err = tls_SSL_get_error ssl n in
        Printf.eprintf "SSL_set_fd error code %s\n"
          (string_of_ssl_error err);
        flush Pervasives.stdout;
      *)
      assert (n <> 1)
  in      
  let () = tls_SSL_set_connect_state ssl in
    { ctx = ctx;
      ssl = ssl
    }
      
let rec tls_read machine () =
  let buf = String.create 9216 in
  let n = tls_SSL_read machine.ssl buf 0 9216 in
    if n <= 0 then
      let err = tls_SSL_get_error machine.ssl n in
        if err = SSL_ERROR_WANT_READ then
          (* continue negotation *)
          tls_read machine ()
        else if err = SSL_ERROR_WANT_WRITE then
          (* TODO: return w/o results *)
          tls_read machine ()
        else (
          let rcvd, _ = tls_SSL_get_shutdown machine.ssl in
            if rcvd then (
              (* normal shutdown *)
              ""
            ) else (* if err = SSL_ERROR_SYSCALL then *) (
              (* broken connection *)
              (*
              Printf.eprintf "SSL_read error code n=%d %s\n" n
                (string_of_ssl_error err);
              flush Pervasives.stderr;
              *)
              ""
            )
        )
    else (* if n > 0 then *)
          String.sub buf 0 n
        
let tls_send machine buf =
  let rec aux_send off =
    let n = tls_SSL_write machine.ssl buf off (String.length buf - off) in
      if n <= 0 then
        let err = tls_SSL_get_error machine.ssl n in
          if err = SSL_ERROR_WANT_READ || err = SSL_ERROR_WANT_WRITE then
            (* continue negotation *)
            aux_send off
          else (
            (*
            Printf.eprintf "SSL_write error code %s\n" (string_of_ssl_error err);
            flush Pervasives.stderr;
            *)
            aux_send off
          )
      else
        if n + off < String.length buf then
          aux_send (off + n)
        else
          ()
  in
    aux_send 0
           
let switch socket =
  let tls = new_machine socket.fd in
    socket.send <- tls_send tls;
    socket.read <- tls_read tls
*)
