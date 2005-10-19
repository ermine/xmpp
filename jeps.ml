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
let make_x_data_field ?label ?var ?value ?required ?options ?type_ () =
   let a1 = match type_ with
      | None -> []
      | Some t -> ["type", match t with
		      | `BOOLEAN -> "boolean"
		      | `FIXED -> "fixed"
		      | `HIDDEN -> "hidden"
		      | `JID_MULTI -> "jid-multi"
		      | `JID_SINGLE -> "jid-single"
		      | `LIST_MULTI -> "list-multi"
		      | `LIST_SINGLE -> "list-single"
		      | `TEXT_MULTI -> "text-multi"
		      | `TEXT_PRIVATE -> "text-private"
		      | `TEXT_SINGLE -> "text-single"] in
   let a2 = match label with
      | None -> a1
      | Some l -> ("label", l) :: a1
   in
   let a3 = match var with
      | None -> a2
      | Some v -> ("var", v) :: a2
   in
   let s1 =  match required with
      | Some true -> [Xmlelement ("required", [], [])]
      | Some false
      | None -> [] in
   let s2 = match value with
      | None -> s1
      | Some v -> make_simple_cdata "value" v :: s1 in
   let s3 = match options with
      | None -> s2
      | Some opts -> 
	   s2 @ List.map (function l, v ->
			     Xmlelement ("option", [("label", l)],
					 [make_simple_cdata "value" v])
                         ) opts in
      Xmlelement ("field", a3, s3)

type xdata_t = [
| `Form
| `Submit
| `Cancel
| `Result
]

let make_x_data ?title ?instructions fieldlist (type_:xdata_t) =
   let s1 =
      match instructions with
	 | None -> fieldlist
	 | Some instr ->
	      make_simple_cdata "instructions" instr:: fieldlist
   in
   let s2 =
      match title with
	 | None -> s1
	 | Some t -> make_simple_cdata "title" t :: s1
   in
      Xmlelement ("x", [("xmlns", "jabber:x:data"); ("type", match type_ with
							| `Form -> "form"
							| `Submit -> "submit"
							| `Cancel -> "cancel"
							| `Result -> "result")],
		  s2)

let get_x_data_type xdata =
   match safe_get_attr_s xdata "type" with
      | "result" -> `Result
      | "cancel" -> `Cancel
      | "form" -> `Form
      | "submit" -> `Submit
      | _ -> raise InvalidProtocol

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
      
let iq_last_reply start_time xml =
   match xml with
      | Xmlelement (_, attrs, _) ->
	   let newattrs = make_attrs_reply ~type_:"result" attrs in
	      Xmlelement ("iq", newattrs, 
			  [Xmlelement ("query", ["xmlns", "jabber:iq:last";
						 "seconds", 
	string_of_int (int_of_float ((Unix.gettimeofday ()) -. start_time))],
				       [])])
      | _ -> raise NonXmlelement

(* jep 90 *)
(*
let iq_time_reply xml =
   let time = Unix.gettimeofday () in
      iq_reply xml
      ~subels:[make_simple_cdata "utc" "";
	       (* make_simple_cdata "tz" ""; *)
	       make_simple_cdata "display" ""]

*)
(*
     <utc>20020910T17:58:35</utc>                                               
     <tz>MDT</tz>                                                               
     <display>Tue Sep 10 12:58:35 2002</display>                                
*)
