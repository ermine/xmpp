(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xmlparser
open Xml
open Xml.Serialization

exception XmlError of string
  
type t = {
  namespaces: (prefix, namespace) Hashtbl.t;
  stack_ns: (qname * (namespace * prefix) list) Stack.t;
  stack: element Stack.t;
  mutable xparser: Xmlparser.parser_t;
  ser: Serialization.t
}

let process_production p stream_start stanza stream_end =
  let add_child el =
    match Stack.pop p.stack with
      | Xmlelement (name, attrs, els) ->
          Stack.push (Xmlelement (name, attrs, el :: els)) p.stack
      | Xmlcdata _ ->
          raise (XmlError "error in xmlstream.ml")
  in
  let rec aux_production (xparser, tag) =
    match tag with
      | Whitespace text ->
          if Stack.length p.stack > 1 then
            add_child (Xmlcdata text);
          aux_production (Xmlparser.parse xparser)
      | Text text ->
          if Stack.length p.stack > 1 then
            add_child (Xmlcdata text)
          else
            raise (XmlError ("text between stanzas '" ^ text ^ "'"));
          aux_production (Xmlparser.parse xparser)
      | Pi _
      | Doctype _
      | Comment _ ->
          aux_production (Xmlparser.parse xparser)
      | StartElement (name, attrs) ->
          let lnss, attrs = split_attrs attrs in
            add_namespaces p.namespaces lnss;
            let attrs =  parse_attrs p.namespaces attrs in
            let qname = parse_qname p.namespaces (split_name name) in
              Stack.push (qname, lnss) p.stack_ns;
              Stack.push (Xmlelement (qname, attrs, [])) p.stack;
              if Stack.length p.stack = 1 then
                stream_start qname attrs;
              aux_production (Xmlparser.parse xparser)
      | EndElement name ->
          if not (Stack.is_empty p.stack) then
            let qname' = parse_qname p.namespaces (split_name name) in
             let (qname, lnss) = Stack.pop p.stack_ns in
               if qname' = qname then (
                 remove_namespaces p.namespaces lnss;
                 match Stack.pop p.stack with
                   | Xmlelement (qname, _attrs, _els) as el ->
                       if qname = qname' then
                         if Stack.is_empty p.stack then (
                           p.xparser <- xparser;
                           stream_end ()
                         ) else if Stack.length p.stack = 1 then (
                           p.xparser <- xparser;
                           match el with
                             | Xmlelement (qname, attrs, els) ->
                                 stanza qname attrs els;
                                 aux_production (Xmlparser.parse xparser)
                             | Xmlcdata _ ->
                                 raise NonXmlelement
                         ) else (
                           add_child el;
                           aux_production (Xmlparser.parse xparser)
                         )
                       else
                         raise (XmlError "unmatched end tag");
                   | Xmlcdata _ ->
                       raise (XmlError "error in xmlstream.ml")
               )
               else
                 raise (XmlError "end tag")
          else
            raise (XmlError "end tag")
      | EndOfBuffer ->
          p.xparser <- xparser;
          ()
      | EndOfData ->
          if Stack.length p.stack < 2 then (
            p.xparser <- xparser;
            stream_end ()
          ) else
            raise End_of_file
  in
    aux_production (Xmlparser.parse p.xparser)

let create default_nss =
  {
    namespaces = Hashtbl.create 1;
    stack_ns = Stack.create ();
    stack = Stack.create ();
    xparser = Xmlparser.create ~encoding:"UTF-8" ();
    ser = Xml.Serialization.create default_nss
  }

let reset p =
  Hashtbl.clear p.namespaces;
  Stack.clear p.stack_ns;
  Stack.clear p.stack;
  p.xparser <- Xmlparser.create ~encoding:"UTF-8" ()

let bind_prefix p prefix namespace =
  Xml.Serialization.bind_prefix p.ser prefix namespace

let add_buffer p buf =
  let newparser = Xmlparser.add_buffer p.xparser buf in
    p.xparser <- newparser

let parse = process_production

let stanza_serialize p el =
  let buf = Buffer.create 30 in
  let out = Buffer.add_string buf in
    aux_serialize [] p.ser out el;
    Buffer.contents buf
  
let stream_header p qname attrs =
  let buf = Buffer.create 30 in
  let out = Buffer.add_string buf in
    out "<?xml version='1.0'?>";
    out "<";
    out (string_of_qname p.ser qname);
    let lnss = local_namespaces p.ser.default_nss p.ser qname attrs in
      if attrs <> [] then (
        out " ";
        out (string_of_list (string_of_attr p.ser) " " attrs)
      );
      if lnss <> [] then (
        out " ";
        out (string_of_list (string_of_ns p.ser) " " p.ser.default_nss)
      );
      out ">";
      Buffer.contents buf
      
let stream_end p qname =
  let buf = Buffer.create 30 in
  let out = Buffer.add_string buf in
    out "</";
    out (string_of_qname p.ser qname);
    out ">";
    Buffer.contents buf
  
