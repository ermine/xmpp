(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open Xml
open Xml.Serialization

module type MONAD =
sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module type INPUT =
sig
  type 'a t
  type stream
  val get : stream -> char option t
end

module IStream (T: Transport.TRANSPORT) =
struct
  include T

  type source = T.socket

  type stream = unit -> int option t

  let set_decoder encname strm =
    if encname = "UTF-8" then () else assert false

  module D = Xmllexer.Decoder (struct include T type stream = T.socket end)

  let make_stream source =
    fun () -> D.decode_utf8 source

  exception XmlError of exn

  let error strm exn =
    fail (XmlError exn)

  let next_char strm eof f =
    strm () >>= function
      | Some u -> f u
      | None -> eof ()
end
  
module XmlStanza (M : MONAD) =
struct
  open M
  type 'a t = 'a M.t

  type data =
    | StartTag of string * (string * string) list * bool
    | EndTag of string
    | Text of string
    | Noop

  type token = data option

  let emit_start_tag name attrs selfclosing =
    return (Some (StartTag (name, attrs, selfclosing)))

  let emit_end_tag name =
    return (Some (EndTag name))

  let emit_doctype doctype =
    return (Some Noop)

  let emit_pi target data =
    return (Some Noop)

  let emit_text text =
    return (Some (Text text))

  let emit_eof () =
    return None
end


module XmlStream (T : Transport.TRANSPORT) =
struct

  module XmlParser = Xmllexer_generic.Make
    (IStream (T))
    (Xmllexer.Encoding)
    (XmlStanza (T))
    
  let (>>=) = T.(>>=)

  module XmlStanza = XmlStanza (T)
  open XmlStanza

  let parse stream_start stanza stream_end strm =
    let namespaces = Hashtbl.create 10 in
    let stack = Stack.create () in
    let stack_ns = Stack.create () in
    let add_child el =
      let (name, attrs, els) = Stack.pop stack in
        Stack.push (name, attrs, (el :: els)) stack
    in
    let strm = XmlParser.S.make_stream strm in
    let next_token = XmlParser.make_lexer strm in
    let rec loop () =
      next_token () >>= function
        | Some t -> (
          match t with
            | StartTag (name, attrs, selfclosing) ->
              let qname, lnss, attrs =
                parse_element_head namespaces name attrs in
                if Stack.is_empty stack then 
                  if selfclosing then (
                    stream_start qname attrs >>= stream_end >>= fun () ->
                    remove_namespaces namespaces lnss;
                    loop ()
                  ) else (
                    Stack.push (qname, attrs, []) stack;
                    Stack.push (qname, lnss) stack_ns;
                    stream_start qname attrs >>= loop
                  )
                else if selfclosing then (
                  remove_namespaces namespaces lnss;
                  if Stack.length stack = 1 then
                    stanza (qname, attrs, []) >>= loop
                  else (
                    add_child (Xmlelement (qname, attrs, []));
                    loop ()
                    )
                  ) else (
                  Stack.push (qname, attrs, []) stack;
                  Stack.push  (qname, lnss) stack_ns;
                  loop ()
                )
                  
            | Text text ->
              add_child (Xmlcdata text);
              loop ()
                
            | EndTag _name ->
              let (qname, lnss) = Stack.pop stack_ns in
                remove_namespaces namespaces lnss;
                let el = Stack.pop stack in
                  if Stack.is_empty stack then
                    stream_end () >>= loop
                  else if Stack.length stack = 1 then
                    stanza el >>= loop
                  else (
                    add_child (Xmlelement el);
                    loop ()
                  )
            | _ -> loop ()
        )
        | None ->
          T.return ()
    in
      loop ()
end

let bind_prefix ser prefix namespace =
  Xml.Serialization.bind_prefix ser prefix namespace

let stanza_serialize ser el =
  let buf = Buffer.create 30 in
  let out = Buffer.add_string buf in
    aux_serialize [] ser out el;
    Buffer.contents buf
  
let stream_header ser qname attrs =
  let buf = Buffer.create 30 in
  let out = Buffer.add_string buf in
    out "<?xml version='1.0'?>";
    out "<";
    out (string_of_qname ser qname);
    let lnss = local_namespaces ser.default_nss ser qname attrs in
      if attrs <> [] then (
        out " ";
        out (string_of_list (string_of_attr ser) " " attrs)
      );
      if lnss <> [] then (
        out " ";
        out (string_of_list (string_of_ns ser) " " ser.default_nss)
      );
      out ">";
      Buffer.contents buf
      
let stream_end ser qname =
  let buf = Buffer.create 30 in
  let out = Buffer.add_string buf in
    out "</";
    out (string_of_qname ser qname);
    out ">";
    Buffer.contents buf
  
