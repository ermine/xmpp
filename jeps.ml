(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Unix
open Xmpp
open Xml

let os = (let f = open_process_in "uname -sr" in
          let answer = input_line f in
             ignore (close_process_in f); answer)

let iq_version_reply name version xml =
   iq_reply ~subels:[make_simple_cdata "name" name;
		     make_simple_cdata "version"
			(Printf.sprintf "%s (Ocaml %s)" 
			    version Sys.ocaml_version);
		     make_simple_cdata "os" os
		    ] xml

type x_data_type = [
| `BOOLEAN
| `FIXED
| `HIDDEN
| `JID_MULTI
| `JID_SINGLE
| `LIST_MULTI
| `LIST_SINGLE
| `TEXT_MULTI
| `TEXT_PRIVATE
| `TEXT_SINGLE
]
let make_x_data_field ?(label="") ?(var="")
      ?(value="") ?(required=false) ?options (type_:x_data_type) =

   let a1 = [("type", 
	      match type_ with
		 | `BOOLEAN -> "boolean"
		 | `FIXED -> "fixed"
		 | `HIDDEN -> "hidden"
		 | `JID_MULTI -> "jid-multi"
		 | `JID_SINGLE -> "jid-single"
		 | `LIST_MULTI -> "list-multi"
		 | `LIST_SINGLE -> "list-single"
		 | `TEXT_MULTI -> "text-multi"
		 | `TEXT_PRIVATE -> "text-private"
		 | `TEXT_SINGLE -> "text-single"
	     )] in
   let a2 = if label <> "" then ("label", label) :: a1 else a1 in
   let a3 = if var <> "" then ("var", var) :: a2 else a2 in

   let s1 = [] in
   let s2 = if required then make_simple_cdata "required" "" :: s1 else s1 in
   let s3 = if value <> "" then make_simple_cdata "value" value :: s2 else s2 in
   let s4 = match options with
         | None -> s3
         | Some opts ->
              s3 @ List.map (function l, v ->
                                Xmlelement ("option", [("label", l)],
                                            [make_simple_cdata "value" v])
                            ) opts in

      Xmlelement ("field", a3, s4)

let make_x_data type_ title instructions fieldlist =
   let s1 =
      if instructions <> ""
      then make_simple_cdata "instructions" instructions :: fieldlist
      else fieldlist
   in
   let s2 =
      if title <> ""
      then make_simple_cdata "title" title :: s1
      else s1
   in
      Xmlelement ("x", [("xmlns", "jabber:x:data"); ("type", type_)], s2)

let get_x_data_fields xdata =
   match xdata with
      | Xmlelement ("x", attrs, subels) ->
           let values = ref [] in
              List.iter
                 (function f ->
                     match f with
                        | Xmlelement ("field", a, s) ->
                             let var = Xml.get_attr_s f "var" in
                             let value = Xml.get_cdata f ~path:["value"] in
                                values := (var, value) :: !values
                           | _ -> ()
                    ) subels;
              !values
      | _ -> raise Not_found

let get_x_data_bool value =
   if value = "0" || value = "no" || value = "false" then false
   else if value = "1" || value = "yes" || value = "true" then true
   else raise Not_found

let make_feature_var feature =
   Xmlelement ("feature", [("var", feature)], [])

let make_disco_item jid ?node name =
   let attr = match node with
      | None ->   [("jid", jid); ("name", name)]
      | Some x -> [("jid", jid); ("node", x); ("name", name)]
   in
      Xmlelement ("item", attr, [])

(* JEP 54 vCard-temp *)

(* "vCard" instead onf "query", so we provide full Xmlelement element  *)
let iq_vcard_query ?from ?lang ~id to_ =
   let a1 = [("id", id); ("to", to_); ("type", "get")] in
   let a2 = match from with
      | None -> a1
      | Some f -> ("from", f) :: a1 in
   let a3 = match lang with
      | None -> a2
      | Some l -> ("xml:lang", l) :: a2 
   in
      Xmlelement ("iq", a3, 
		  [Xmlelement ("vCard", ["xmlns", "vcard-temp"], [])])
      
