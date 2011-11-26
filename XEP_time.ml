(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *
 *  XEP-0090: Legacy Entity Time
 * Version:  1.2
 *)

open Xml
  
let ns_time = Some "jabber:iq:time"

type t = {
  utc : string;
  tz : string;
  display : string
}

let encode t =
  make_element (ns_time, "query") []
    (List.fold_left (fun acc -> function
                       | None -> acc
                       | Some el -> el :: acc
                    ) [make_simple_cdata (ns_time, "utc") t.utc]
       [(if t.tz = "" then None else
           Some (make_simple_cdata (ns_time, "tz") t.tz));
        (if t.display = "" then None else
           Some (make_simple_cdata (ns_time, "display") t.display))]
    )
    
let decode el =
  let els = get_children el in
  let utc =
    try get_cdata (get_element (ns_time, "utc") els) with Not_found -> "" in
  let tz =
    try get_cdata (get_element (ns_time, "tz") els) with Not_found -> "" in
  let display =
    try get_cdata (get_element (ns_time, "display") els) with Not_found -> "" in
    {utc = utc; tz = tz; display = display}

let make_iq_get () =
  make_element (ns_time, "query") [] []
