(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open Xml
open Xml.Serialization

module type MONAD =
sig
  type 'a t
  val return : 'a  -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module type TRANSPORT =
sig
  type 'a z
  type socket
  val get : socket -> char option z
  val send : socket -> string -> unit z
end

module IStream (M : MONAD) (T : TRANSPORT with type 'a z = 'a M.t) =
struct
  include M
  include T

  type source = T.socket

  type stream = unit -> int option t

  let set_decoder encname strm =
    if encname = "UTF-8" then () else assert false

  module D = Xmllexer.Decoder (struct include M
                                      include T type stream = T.socket end)

  let make_stream source =
    fun () -> D.decode_utf8 source

  exception XmlError of string

  let error strm exn =
    fail (XmlError (Printexc.to_string exn))

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


module XmlStream (M: MONAD) (T : TRANSPORT with type 'a z = 'a M.t) =
struct
  type p = {
    namespaces : (string, Xml.namespace) Hashtbl.t;
    stack : (Xml.qname * Xml.attribute list * Xml.element list) Stack.t;
    stack_ns : (Xml.qname * (Xml.namespace * Xml.prefix) list) Stack.t
  }
    
  let create () = {
    namespaces = Hashtbl.create 10;
    stack = Stack.create ();
    stack_ns = Stack.create ()
  }

  module XmlParser = Xmllexer_generic.Make
    (IStream (M)(T))
    (Xmllexer.Encoding)
    (XmlStanza (M))

  let make_stream = XmlParser.S.make_stream
  let make_lexer = XmlParser.make_lexer

  let (>>=) = M.(>>=)

  module XmlStanza = XmlStanza (M)
  open XmlStanza

  let parse p next_token stream_start stanza stream_end strm =
    let add_child el =
      let (name, attrs, els) = Stack.pop p.stack in
        Stack.push (name, attrs, (el :: els)) p.stack
    in
    let rec loop () =
      next_token () >>= function
        | Some t -> (
          match t with
            | StartTag (name, attrs, selfclosing) ->
              let qname, lnss, attrs =
                parse_element_head p.namespaces name attrs in
                if Stack.is_empty p.stack then 
                  if selfclosing then (
                    stream_start qname attrs >>= stream_end >>= fun () ->
                    remove_namespaces p.namespaces lnss;
                    loop ()
                  ) else (
                    Stack.push (qname, attrs, []) p.stack;
                    Stack.push (qname, lnss) p.stack_ns;
                    stream_start qname attrs >>= loop
                  )
                else if selfclosing then (
                  remove_namespaces p.namespaces lnss;
                  if Stack.length p.stack = 1 then
                    stanza (qname, attrs, []) >>= loop
                  else (
                    add_child (Xmlelement (qname, attrs, []));
                    loop ()
                    )
                  ) else (
                  Stack.push (qname, attrs, []) p.stack;
                  Stack.push  (qname, lnss) p.stack_ns;
                  loop ()
                )
                  
            | Text text ->
              add_child (Xmlcdata text);
              loop ()
                
            | EndTag _name ->
              let (qname, lnss) = Stack.pop p.stack_ns in
                remove_namespaces p.namespaces lnss;
                let (q, a, els) = Stack.pop p.stack in
                  if Stack.is_empty p.stack then
                    stream_end () >>= loop
                  else if Stack.length p.stack = 1 then
                    stanza (q, List.rev a, List.rev els) >>= loop
                  else (
                    add_child (Xmlelement (q, List.rev a, List.rev els));
                    loop ()
                  )
            | _ -> loop ()
        )
        | None ->
          M.return ()
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
  
