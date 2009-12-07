(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *
 * XEP-0092: Software Version
 * Version: 1.1
 *)

open Xml
open XMPP
open StanzaError

let ns_version = Some "jabber:iq:version"

type t = {
  name: string;    (* required *)
  version: string; (* required *)
  os: string;      (* optional *)
}

let encode t =
  make_element (ns_version, "query") []
    (List.fold_left (fun acc (k,v) ->
                       if v <> "" then
                         make_simple_cdata (ns_version, k) v :: acc
                       else
                         acc
                    ) [] ["name", t.name;
                          "os", t.os;
                          "version", t.version])
    
let decode el =
  try
    let els = get_children el in
    let name = get_cdata (get_element (ns_version, "name") els) in
    let version = get_cdata (get_element (ns_version, "version") els) in
    let os =
      try get_cdata (get_element (ns_version, "os") els) with Not_found -> "" in
      Some {name = name; version = version; os = os}
  with Not_found -> None

let make_iq_get () =
  make_element (ns_version, "query") [] []
