(*
 * (c) 2004-2011 Anastasia Gornostaeva
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
    
let decode _attrs els =
  let result =
    List.fold_left (fun item -> function
      | Xmlelement ((ns_version, "name"), _, _) as el ->
        let value = get_cdata el in
          {item with name = value}
      | Xmlelement ((ns_version, "version"), _, _) as el ->
        let value = get_cdata el in
          {item with version = value}
      | Xmlelement ((ns_version, "os"), _, _) as el ->
        let value = get_cdata el in
          {item with os = value}
      | _ -> item
    ) {name = "";
       version = "";
       os = ""
      } els in
    if result.name = "" && result.version = "" then
      None
    else
      Some result

let get xmpp ?jid_from ?jid_to ?lang ?(error_callback=ignore) callback =
  let callback ev _jid_from _jid_to _lang () =
    match ev with
      | IQResult el -> (
        match el with
          | Some (Xmlelement ((ns_version, "query"), attrs, els)) ->
            callback ?jid_from ?jid_to ?lang (decode attrs els)
          | _ ->
            callback ?jid_from ?jid_to ?lang None
      )
      | IQError err ->
        error_callback err
  in
    make_iq_request xmpp ?jid_from ?jid_to ?lang
      (IQGet (make_element (ns_version, "query") [] []))
    callback
  
let iq_request ~get =
  (fun ev jid_from jid_to lang () ->
    match ev with
      | IQGet _el ->
        let t = get ?jid_from ?jid_to ?lang () in
        let el = encode t in
          IQResult (Some el)
      | IQSet _el ->
        raise BadRequest
  )
