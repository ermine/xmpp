(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *
 *  XEP-0039: Statistics Gathering
 *  Version: 0.6.0
 *)

module Make (X : XMPP.S) =
struct
  open Xml
  
  let ns_stats = Some "http://jabber.org/protocol/stats"
  
  type t = {
    name : string;
    units : string;
    value : string
  }
    
  let encode data =
    make_element (ns_stats, "query") []
      (List.map (fun t ->
        make_element (ns_stats, "stat")
          [make_attr "name" t.name;
           make_attr "units" t.units;
           make_attr "value" t.value]
          []
       ) data)
      
  let decode el =
    List.fold_left (fun acc -> function
      | Xmlelement (qname, attrs, _) ->
        if qname = (ns_stats, "stat") then
          let name = safe_get_attr_value "name" attrs in
          let units = safe_get_attr_value "units" attrs in
          let value = safe_get_attr_value "value" attrs in
            { name = name; units = units; value = value } :: acc
        else
          acc
      | Xmlcdata _ ->
        acc
    ) [] (get_children el)
      
      
  let make_iq_get alist =
    make_element (ns_stats, "query") []
      (List.map (fun name ->
        make_element (ns_stats, "stat") [make_attr "name" name]
          []) alist)
 end
