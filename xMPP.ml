(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open XMPP_types
  
module Network : NETWORK =
struct
  open Unix
       
  type 'a t = 'a
  let return x = x
  let (>>=) v f =  f v
  let fail = raise
    
  type in_channel = Pervasives.in_channel
  type out_channel = Pervasives.out_channel

  let connect host port =
    let inet_addr =
      try Unix.inet_addr_of_string host
      with Failure("inet_addr_of_string") ->
        (Unix.gethostbyname host).Unix.h_addr_list.(0) in
    let sock_addr = Unix.ADDR_INET (inet_addr, port) in
      open_connection sock_addr
        
  let read inch =
    let string = String.create 8193 in
    let size = input inch string 0 8192 in
      if size = 0 then (
        close_in inch;
        return ""
      ) else (
        let s = String.sub string 0 size in
          print_endline s;
          return s
      )
          
  let send ouch string =
    print_endline string;
    output_string ouch string;
    flush ouch;
    return ()
end      
      
module M = XMPP_generic.Make(Network)
     
include M
