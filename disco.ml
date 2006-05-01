(*                                                                          *)
(* (c) 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>                    *)
(*                                                                          *)

open Xml
open Xmpp

let make_feature_var feature =
   Xmlelement ("feature", [("var", feature)], [])

let make_disco_item jid ?node name =
   let attr = match node with
      | None ->   [("jid", jid); ("name", name)]
      | Some x -> [("jid", jid); ("node", x); ("name", name)]
   in
      Xmlelement ("item", attr, [])

let make_disco_info ~category ~type_ ~name ~features () =
   Xmlelement ("identity", ["category", category; "type", type_; "name", name],
	       []) ::
      List.map (fun feature -> make_feature_var feature) features

let make_disco_item jid ?node name = 
   let attr = match node with 
      | None ->   [("jid", jid); ("name", name)]
      | Some x -> [("jid", jid); ("node", x); ("name", name)]
   in
      Xmlelement ("item", attr, [])

