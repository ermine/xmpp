(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xmlparser
open Light_xml

exception XmlError of string
  
type t =
  | StreamStart of string * (string * string) list
  | StreamEnd
  | Stanza of element
  | Continue

let process_production stack state =
  let add_child el =
    match Stack.pop stack with
      | Xmlelement (name, attrs, els) ->
          Stack.push (Xmlelement (name, attrs, el :: els)) stack
      | Xmlcdata _ ->
          raise (XmlError "error in xmlstream.ml")
  in
  let rec aux_production (state, tag) =
    match tag with
      | Whitespace text ->
          if Stack.length stack > 1 then
            add_child (Xmlcdata text);
          aux_production (Xmlparser.parse state)
      | Text text ->
          if Stack.length stack > 1 then
            add_child (Xmlcdata text)
          else
            raise (XmlError "text between stanzas");
          aux_production (Xmlparser.parse state)
      | Pi _
      | Doctype _
      | Comment _ ->
          aux_production (Xmlparser.parse state)
      | StartElement (name, attrs) ->
          Stack.push (Xmlelement (name, attrs, [])) stack;
          if Stack.length stack = 1 then
            ((state, stack), StreamStart (name, attrs))
          else
            aux_production (Xmlparser.parse state)
      | EndElement name ->
          if Stack.length stack > 0 then
            match Stack.pop stack with
              | Xmlelement (name', _attrs, _els) as el ->
                  if name = name' then
                    if Stack.length stack = 0 then
                      ((state, stack), StreamEnd)
                    else if Stack.length stack = 1 then
                      ((state, stack), Stanza el)
                    else (
                      add_child el;
                      aux_production (Xmlparser.parse state)
                    )
                  else
                    raise (XmlError "unmatched end tag");
              | Xmlcdata _ ->
                  raise (XmlError "error in xmlstream.ml")
          else
            raise (XmlError "end tag")
      | EndOfBuffer ->
          ((state, stack), Continue)
      | EndOfData ->
          if Stack.length stack < 2 then
            ((state, stack), StreamEnd)
          else
            raise End_of_file
  in
    aux_production (Xmlparser.parse state)

let create () =
  let stack = Stack.create () in
  let state = Xmlparser.create ~encoding:"UTF-8" () in
    (state, stack)

let add_buffer p buf =
  let (state, stack) = p in
  let newstate = Xmlparser.add_buffer state buf in
    (newstate, stack)

let parse p =
  let (state, stack) = p in
    process_production stack state
