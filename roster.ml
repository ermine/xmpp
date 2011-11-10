(*
 * (c) 2004-2011 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
open XMPP
open JID

let ns_roster = Some "jabber:iq:roster"

type ask_t =
  | AskSubscribe

type subscription_t =
  | SubscriptionNone
  | SubscriptionBoth
  | SubscriptionFrom
  | SubscriptionRemove
  | SubscriptionTo

type item = {
  group : string list;
  approved : bool;
  ask : ask_t option;
  jid : JID.t;
  name : string;
  subscription : subscription_t;
}

let decode attrs els =
  let ver =
    try Some (get_attr_value "ver" attrs)
    with Not_found -> None in
  let items =
    List.fold_left (fun acc -> function
      | Xmlelement ((ns_roster, "item"), attrs, els) ->
        let item =
          List.fold_left (fun item -> function
            | ((None, "approved"), value) ->
              if value = "true" then
                {item with approved = true}
              else
                item
            | ((None, "ask"), value) ->
              if value = "subscribe" then
                {item with ask = Some AskSubscribe}
              else
                item
            | ((None, "jid"), value) ->
                (* TODO validity and verify *)
              {item with jid = try JID.of_string value with _ -> item.jid}
            | ((None, "name"), value) ->
              {item with name = value}
            | ((None, "subscription"), value) ->
              let value =
                match value with
                  | "none" -> SubscriptionNone
                  | "both" -> SubscriptionBoth
                  | "from" -> SubscriptionFrom
                  | "to" -> SubscriptionTo
                  | "remove" -> SubscriptionRemove
                  | _ -> SubscriptionNone
              in
                {item with subscription = value}
            | _ -> item
          ) {group = [];
             approved = false;
             ask = None;
             jid = {
               node = "";
               lnode = "";
               domain = "";
               ldomain = "";
               resource = "";
               lresource = ""
             };
             name = "";
             subscription = SubscriptionNone
            } attrs in
        let item =
          List.fold_left (fun item -> function
            | Xmlelement ((ns_roster, "group"), _, _) as el ->
              let group = get_cdata el in
                {item with group = group :: item.group}
            | _ -> item
          ) item els in
          if item.jid.node = "" || item.jid.domain = "" then
            acc
          else
            item :: acc
    ) [] els in
    (ver, List.rev items)

let get xmpp ?jid_from ?jid_to ?lang ?(error_callback=ignore) callback =
  let callback ev jid_from jid_to lang () =
    match ev with
      | IQResult el -> (
        match el with
          | Some (Xmlelement ((ns_roster, "query"), attrs, els)) ->
            let ver, items = decode attrs els in
              callback ?jid_from ?jid_to ?lang ?ver items
          | _ ->
            callback ?jid_from ?jid_to ?lang ?ver:None []
      )
      | IQError err ->
        error_callback err
  in
    make_iq_request xmpp ?jid_from ?jid_to ?lang
      (IQGet (make_element (ns_roster, "query") [] []))
      callback
