(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xmlparser
open Light_xml

exception XmlError of string
  
type t =
  | StreamError of element list
  | StreamEnd
  | Element of element

let create read_raw =

  let rec get_childs name attrs els (state, tag) =
    match tag with
      | Whitespace text
      | Text text ->
          get_childs name attrs (Xmlcdata text :: els) (Xmlparser.parse state)
      | StartElement (name', attrs') ->
          let (state, el) =
            get_childs name' attrs' [] (Xmlparser.parse state) in
            get_childs name attrs (el :: els) (Xmlparser.parse state)
      | EndElement name' ->
          if name = name' then
            (state, Xmlelement (name, attrs, List.rev els))
          else
            failwith "Unmatched end element"
      | Pi _
      | Comment _ ->
          get_childs name attrs els (Xmlparser.parse state)
      | EndOfBuffer ->
          let buf = read_raw () in
          let newstate =
            if buf = "" then
              Xmlparser.set_finish state
            else
              Xmlparser.add_buffer state buf
          in
            get_childs name attrs els (Xmlparser.parse newstate)
      | EndOfData ->
          raise End_of_file
      | Doctype _ ->
          failwith "Unexpected tag in epilogue"
  in

  let rec get_stanza (state, tag) =
    match tag with
      | Pi _
      | Comment _
      | Doctype _
      | Text _
      | Whitespace _ ->
          get_stanza (parse state)
      | StartElement (name, attrs) ->
          let (state, el) =
            get_childs name attrs [] (Xmlparser.parse state) in
            ((state, get_stanza), Element el)
      | EndElement name ->
          if name = "stream:stream" then
            ((state, process_prolog), StreamEnd)
          else
            failwith ("Unexpected end element " ^ name)
      | EndOfBuffer ->
          let buf = read_raw () in
          let newstate =
            if buf = "" then
              Xmlparser.set_finish state
            else
              Xmlparser.add_buffer state buf
          in
            get_stanza (Xmlparser.parse newstate)
      | EndOfData ->
          ((state, process_prolog), StreamEnd)

  and process_prolog (state, tag) =
   match tag with
      | Pi _
      | Comment _
      | Doctype _ ->
          process_prolog (parse state)
      | StartElement (name, attrs) ->
          if name = "stream:stream" then
            ((state, get_stanza),
             Element (Xmlelement ("stream:stream", attrs, [])))
          else
            raise (XmlError "expected stream:stream")
      | EndElement name ->
          failwith ("Unexpected </" ^ name ^ ">")
      | Whitespace _spaces ->
          process_prolog (parse state)
      | Text _ ->
          failwith "Unexpected text"
      | EndOfBuffer ->
          let buf = read_raw () in
          let newstate =
            if buf = "" then
              Xmlparser.set_finish state
            else
              Xmlparser.add_buffer state buf
          in
            process_prolog (Xmlparser.parse newstate)
      | EndOfData ->
          ((state, process_prolog), StreamEnd)
  in

  let state = Xmlparser.create ~encoding:"UTF-8" () in
  let p = ref (state, process_prolog) in
    fun () ->
      let state, callback = !p in
      let (ss, tag) = callback (Xmlparser.parse state) in
        p := ss;
        tag
