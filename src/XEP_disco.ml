(*
 * (c) 2006-2012 Anastasia Gornostaeva
 *)

module Make (X : XMPP.S) =
struct
  open Xml
  open X

  let ns_disco_info = Some "http://jabber.org/protocol/disco#info"
  let ns_disco_items = Some "http://jabber.org/protocol/disco#items"

  type category =
    | Account                                                                   
    | Auth                                                                      
    | Automation                                                                
    | Client                                                                    
    | Collaboration                                                             
    | Component                                                                 
    | Conference                                                                
    | Directory                                                                 
    | Gateway                                                                   
    | Headline                                                                 
    | Hierarchy                                                                
    | Proxy                                                                    
    | Pubsub                                                                   
    | Server                                                                   
    | Store                                                                    
        
  let make_feature_var feature =
    Xmlelement ((ns_disco_info, "feature"),
                [make_attr "var" feature], [])
      
  let make_disco_item jid ?node name =
    let attr = match node with
      | None ->   [make_attr "jid" jid;
                   make_attr "name" name]
      | Some x -> [make_attr "jid" jid;
                   make_attr "node" x;
                   make_attr "name" name]
    in
      Xmlelement ((ns_disco_items, "item"), attr, [])
        
  let make_disco_info ~category ~type_ ~name ~features () =
    Xmlelement ((ns_disco_info, "identity"),
                [make_attr "category" category;
                 make_attr "type" type_;
                 make_attr "name" name],
	              []) ::
      List.map (fun feature -> make_feature_var feature) features
      
  let make_disco_item jid ?node name = 
    let attr = match node with 
      | None ->   [make_attr "jid" jid; make_attr "name" name]
      | Some x -> [make_attr "jid" jid; make_attr "node" x;
                   make_attr "name" name]
    in
      Xmlelement ((ns_disco_items, "item"), attr, [])
        
end
