(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

open Xml

module type MONAD =
sig
  type 'a t
  val return : 'a  -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module IStream (M : MONAD) =
struct
  include M

  type stream = {
    read : string -> int -> int -> int M.t;
    buf : string;
    mutable i : int;
    mutable len : int;
    mutable is_final : bool
  }

  let refill s =
    s.read s.buf 0 8192 >>= fun size ->
    if size = 0 then
      s.is_final <- true
    else (
      s.i <- 0;
      s.len <- size
    );
    return ()
      
  exception IllegalCharacter
    
  let rec get s =
    if s.i < s.len then
      let ch1 = s.buf.[s.i] in
        s.i <- s.i + 1;
        match ch1 with
          | '\000'..'\127' -> return (Some (Char.code ch1))

          | '\192'..'\223' ->
            let rec cont () =
              if s.i < s.len then
                let ch2 = s.buf.[s.i] in
                  s.i <- s.i + 1;
                  let n1 = Char.code ch1 in
                  let n2 = Char.code ch2 in
                    if (n2 lsr 6 != 0b10) then fail IllegalCharacter
                    else
                      let code = ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f) in
                        return (Some (code))
              else if s.is_final then
                fail IllegalCharacter
              else
                refill s >>= cont
            in
              cont ()
                
          | '\224'..'\239' ->
            let rec cont1 () =
              if s.i < s.len then
                let ch2 = s.buf.[s.i] in
                  s.i <- s.i + 1;
                  let rec cont2 () =
                    if s.i < s.len then
                      let ch3 = s.buf.[s.i] in
                        s.i <- s.i + 1;
                        let n1 = Char.code ch1
                        and n2 = Char.code ch2
                        and n3 = Char.code ch3 in
                          if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then
                            fail IllegalCharacter
                          else
                            let code = 
                              ((n1 land 0x0f) lsl 12) lor
                                ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
                            in
                              if (code >= 0xd800) && (code <= 0xdf00) then
                                fail IllegalCharacter
                              else return (Some (code))
                    else if s.is_final then
                      fail IllegalCharacter
                    else
                      refill s >>= cont2
                  in
                    cont2 ()
              else if s.is_final then
                fail IllegalCharacter
              else
                refill s >>= cont1
            in
              cont1 ()
                      
          | '\240'..'\247' -> 
            let rec cont1 () =
              if s.i < s.len then
                let ch2 = s.buf.[s.i] in
                  s.i <- s.i + 1;
                  let rec cont2 () =
                    if s.i < s.len then
                      let ch3 = s.buf.[s.i] in
                        s.i <- s.i + 1;
                        let rec cont3 () =
                          if s.i < s.len then
                            let ch4 = s.buf.[s.i] in
                              s.i <- s.i + 1;
                              let n1 = Char.code ch1
                              and n2 = Char.code ch2
                              and n3 = Char.code ch3
                              and n4 = Char.code ch4 in
                                if (n2 lsr 6 != 0b10) ||
                                  (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10) then
                                  fail IllegalCharacter
                                else
                                  return (Some (((n1 land 0x07) lsl 18) lor
                                                   ((n2 land 0x3f) lsl 12) lor
                                                   ((n3 land 0x3f) lsl 6)
                                                lor (n4 land 0x3f)))
                          else if s.is_final then
                            fail IllegalCharacter
                          else
                            refill s >>= cont3
                        in
                          cont3 ()
                    else if s.is_final then
                      fail IllegalCharacter
                    else
                      refill s >>= cont2
                  in
                    cont2 ()
              else if s.is_final then
                fail IllegalCharacter
              else
                refill s >>= cont1
            in
              cont1 ()
                
          | _ ->
            fail IllegalCharacter

    else if s.is_final then
      return None
    else
      refill s >>= fun () -> get s

  let set_decoder encname strm =
    if encname = "UTF-8" then () else assert false

  let make_stream read =
    { read = read;
      buf = String.create 8192;
      i = 0;
      len = 0;
      is_final = false
    }


  exception XmlError of string

  let error ?stream exn =
    fail (XmlError (Printexc.to_string exn))

  let next_char strm eof f =
    get strm >>= function
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


module type S = functor (M : MONAD) ->
sig
  type p
  exception XmlError of string
  val create : (string -> int -> int -> int M.t) -> p
  val parse : p -> (Xml.qname -> Xml.attribute list -> unit M.t) ->
    (Xml.qname * Xml.attribute list * Xml.element list -> unit M.t) ->
    (unit -> unit M.t) -> unit M.t
  val reset : p -> (string -> int -> int -> int M.t) option -> unit
end
  
module XmlStream (M : MONAD) =
struct
  module I = IStream (M)
  module XmlParser = Xmllexer_generic.Make
    (I)
    (Xmllexer.Encoding)
    (XmlStanza (M))

  type stream = I.stream

  exception Exn_msg = XmlParser.Exn_msg
  exception Exn_EOF = XmlParser.Exn_EOF
  exception Exn_ExpectedChar = XmlParser.Exn_ExpectedChar
  exception Exn_ExpectedSpace = XmlParser.Exn_ExpectedSpace
  exception Exn_CharToken = XmlParser.Exn_CharToken
  exception Error_XMLDecl = XmlParser.Error_XMLDecl

  exception XmlError = I.XmlError

  type p = {
    namespaces : (string, Xml.namespace) Hashtbl.t;
    stack : (Xml.qname * Xml.attribute list * Xml.element list) Stack.t;
    stack_ns : (Xml.qname * (Xml.namespace * Xml.prefix) list) Stack.t;
    state : XmlParser.state;
    mutable strm : XmlParser.S.stream;
  }

  let create read = {
    namespaces = Hashtbl.create 10;
    stack = Stack.create ();
    stack_ns = Stack.create ();
    state = XmlParser.create_state ();
    strm = I.make_stream read
  }

  let reset p read =
    Hashtbl.clear p.namespaces;
    Stack.clear p.stack;
    Stack.clear p.stack_ns;
    XmlParser.reset p.state;
    match read with
      | None -> ()
      | Some v -> p.strm <- I.make_stream v

  let (>>=) = M.(>>=)

  module XmlStanza = XmlStanza (M)
  open XmlStanza

  let parse p stream_start stanza stream_end =
    let add_child el =
      let (name, attrs, els) = Stack.pop p.stack in
        Stack.push (name, attrs, (el :: els)) p.stack
    in
    let rec loop () =
      XmlParser.lexer p.state p.strm >>= function
        | Some t -> (
          match t with
            | StartTag (name, attrs, selfclosing) ->
              let qname, lnss, attrs =
                parse_element_head p.namespaces name attrs in
                if Stack.is_empty p.stack then 
                  if selfclosing then (
                    stream_start qname attrs >>= stream_end >>= fun () ->
                    remove_namespaces p.namespaces lnss;
                    M.return ()
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
                    stream_end () >>= fun () -> M.return ()
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

module IterStream =
struct
  include XmllexerE.IterMonad

  type stream = input -> int option t
      
  let set_decoder encname strm = ()

  let make_stream () = get

  exception XmlError of string

  let error ?stream exn =
    fail (XmlError (Printexc.to_string exn))

  let next_char strm eof f =
    Continue (fun source -> strm source >>= function
      | Some u -> f u
      | None -> eof ()
    )
end

module XmlStreamIE (M : MONAD) =
struct
  module IE = XmllexerE.IterMonad
  module X = XmlStanza (struct include IE
                               let catch f1 f2 = try f1 () with exn -> f2 exn
  end)

  module XmlParser = Xmllexer_generic.Make
    (IterStream)
    (Xmllexer.Encoding)
    (X)

  open IE
  open X

  exception Exn_msg = XmlParser.Exn_msg
  exception Exn_EOF = XmlParser.Exn_EOF
  exception Exn_ExpectedChar = XmlParser.Exn_ExpectedChar
  exception Exn_ExpectedSpace = XmlParser.Exn_ExpectedSpace
  exception Exn_CharToken = XmlParser.Exn_CharToken
  exception Error_XMLDecl = XmlParser.Error_XMLDecl

  exception XmlError = IterStream.XmlError

  type p = {
    namespaces : (string, Xml.namespace) Hashtbl.t;
    stack : (Xml.qname * Xml.attribute list * Xml.element list) Stack.t;
    stack_ns : (Xml.qname * (Xml.namespace * Xml.prefix) list) Stack.t;
    state : XmlParser.state;
    strm : IterStream.stream;
    mutable read : string -> int -> int -> int M.t;
    mutable cont : IE.input -> X.token IE.t;
    source : IE.input
  }
    
  let reset p read =
    Hashtbl.clear p.namespaces;
    Stack.clear p.stack;
    Stack.clear p.stack_ns;
    XmlParser.reset p.state;
    match read with
      | None -> ()
      | Some v -> p.read <- v

  let (>>=) = M.(>>=)
  let return = M.return

  let parse p stream_start stanza stream_end =
    let add_child el =
      let (name, attrs, els) = Stack.pop p.stack in
        Stack.push (name, attrs, (el :: els)) p.stack
    in
    let rec loop = function
      | Return (Some t) -> (
        match t with
          | StartTag (name, attrs, selfclosing) ->
            let qname, lnss, attrs =
              parse_element_head p.namespaces name attrs in
              if Stack.is_empty p.stack then 
                if selfclosing then (
                  stream_start qname attrs >>= stream_end >>= fun () ->
                  remove_namespaces p.namespaces lnss;
                  loop (Return None)
                ) else (
                  Stack.push (qname, attrs, []) p.stack;
                  Stack.push (qname, lnss) p.stack_ns;
                  stream_start qname attrs >>= fun () ->
                  loop (XmlParser.lexer p.state p.strm)
                )
              else if selfclosing then (
                remove_namespaces p.namespaces lnss;
                if Stack.length p.stack = 1 then
                  stanza (qname, attrs, []) >>= fun () ->
                loop (XmlParser.lexer p.state p.strm)
                else (
                  add_child (Xmlelement (qname, attrs, []));
                  loop (XmlParser.lexer p.state p.strm)
                )
              ) else (
                Stack.push (qname, attrs, []) p.stack;
                Stack.push  (qname, lnss) p.stack_ns;
                loop (XmlParser.lexer p.state p.strm)
              )
          | Text text ->
            add_child (Xmlcdata text);
            loop (XmlParser.lexer p.state p.strm)
          | EndTag _name ->
            let (qname, lnss) = Stack.pop p.stack_ns in
              remove_namespaces p.namespaces lnss;
              let (q, a, els) = Stack.pop p.stack in
                if Stack.is_empty p.stack then
                  stream_end () >>= fun () -> loop (Return None)
                else if Stack.length p.stack = 1 then
                  stanza (q, List.rev a, List.rev els) >>= fun () ->
                loop (XmlParser.lexer p.state p.strm)
                else (
                  add_child (Xmlelement (q, List.rev a, List.rev els));
                  loop (XmlParser.lexer p.state p.strm)
                )
          | _ -> loop (XmlParser.lexer p.state p.strm)
      )
      | Return None -> fail End_of_file
      | Continue cont ->
        if is_empty p.source then (
          p.cont <- cont;
          return ()
        )
        else
          loop (cont p.source)
    in
      p.read p.source.buf 0 8192 >>= fun size ->
    if size = 0 then
      p.source.is_final <- true
    else (
      p.source.i <- 0;
      p.source.len <- size
    );
      loop (p.cont p.source)


  let create read =
    let state = XmlParser.create_state () in
    let strm = IterStream.make_stream () in
      {
        namespaces = Hashtbl.create 10;
        stack = Stack.create ();
        stack_ns = Stack.create ();
        state = state;
        strm = strm;
        read = read;
        cont = (fun source -> XmlParser.lexer state strm);
        source = IE.make_chunk 8192
      }
end


open Xml.Serialization

let bind_prefix ser prefix namespace =
  Xml.Serialization.bind_prefix ser prefix namespace

let stanza_serialize ser el =
  let buf = Buffer.create 30 in
  let out = Buffer.add_string buf in
    Xml.Serialization.aux_serialize [] ser out el;
    Buffer.contents buf
  
let stream_header ser qname attrs =
  let buf = Buffer.create 30 in
  let out = Buffer.add_string buf in
    out "<?xml version='1.0'?>";
    out "<";
    out (Xml.Serialization.string_of_qname ser qname);
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
  
