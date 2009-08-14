(*
 * (c) 2007-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Printf
open Mltls

type machine = {
  ctx: tls_SSL_CTX;
  ssl: tls_SSL
}

let new_machine fd certificate =
  let ctx = tls_SSL_CTX_new SSLv23_method in
  let () =
    let n = tls_SSL_CTX_use_certificate_file ctx 
      certificate SSL_FILETYPE_PEM in
      assert (n = 1)
  in
  let () =
    let n = tls_SSL_CTX_use_PrivateKey_file ctx 
      certificate SSL_FILETYPE_PEM in
      assert (n = 1)
  in
  let () =
    let n = tls_SSL_CTX_check_private_key ctx in
      assert (n = 1)
  in
  let () =
    let n = tls_SSL_CTX_set_default_verify_paths ctx in
      assert (n = 1)
  in
  let () =
    let verify_callback (preverify_ok:int) 
        (x509_store_ctx:tls_X509_STORE_CTX) =
      print_endline "VERIFY CALLBACK";
      flush Pervasives.stdout;
      1
    in
      tls_SSL_CTX_set_verify ctx
        [SSL_VERIFY_PEER; SSL_VERIFY_CLIENT_ONCE]
        verify_callback
  in
  let ssl = tls_SSL_new ctx in
  let n = tls_SSL_set_fd ssl fd in
  let () =
    if n <> 1 then
      let err = tls_SSL_get_error ssl n in
        eprintf "SSL_set_fd error code %s\n" (string_of_ssl_error err);
        assert (n = 1) in
  let () = tls_SSL_set_connect_state ssl in
    { ctx = ctx;
      ssl = ssl; }
      
let read machine f =
  let buf = String.create 9216 in
  let n = tls_SSL_read machine.ssl buf 0 9216 in
    if n <= 0 then
      let err = tls_SSL_get_error machine.ssl n in
        if err = SSL_ERROR_WANT_READ || err = SSL_ERROR_WANT_WRITE then
          (* continue negotation *)
          ()
        else (
          let rcvd, _ = tls_SSL_get_shutdown machine.ssl in
            if rcvd then (
              (* normal shutdown *)
              printf "normal shutdown\n";
              f buf 0
            ) else (* if err = SSL_ERROR_SYSCALL then *) (
              (* broken connection *)
              eprintf "SSL_read error code n=%d %s\n" n
                (string_of_ssl_error err);
              flush stderr;
              f buf 0
            )
        )
    else
      f buf n
        
let send machine buf pos len =
   let n = tls_SSL_write machine.ssl buf pos len in
      if n <= 0 then
         let err = tls_SSL_get_error machine.ssl n in
            if err = SSL_ERROR_WANT_READ || err = SSL_ERROR_WANT_WRITE then
               (* continue negotation *)
               0
            else (
               eprintf "SSL_write error code %s\n" (string_of_ssl_error err);
               flush Pervasives.stderr;
               n
            )
      else
         n
           
let switch socket =
  let tls = new_machine socket.fd "cert.pem" in
    socket.send <- send tls;
    socket.read <- recv tls
           
