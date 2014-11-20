(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *
 * XWP-0045 Multi-User Chat
 * Version: 1.24
 *)

module Make (X : XMPP.S) =
struct
  open Xml
  open JID
  open X

  let ns_muc = Some "http://jabber.org/protocol/muc"
  let ns_muc_user = Some "http://jabber.org/protocol/muc#user"
  let ns_muc_admin = Some "http://jabber.org/protocol/muc#admin"
  let ns_muc_owner = Some "http://jabber.org/protocol/muc#owner"
  let ns_muc_unique = Some "http://jabber.org/protocol/muc#unique"
  
  exception InvalidProtocol

  type role =
    | RoleModerator
    | RoleParticipant
    | RoleVisitor
    | RoleNone

  type affiliation =
    | AffiliationOwner
    | AffiliationAdmin
    | AffiliationMember
    | AffiliationOutcast
    | AffiliationNone

  let maybe f = function None -> None | Some v -> Some (f v)

  let get_attr_value_option name attrs =
    try Some (get_attr_value name attrs) with Not_found -> None
              
  let get_subcdata_option qname el =
    try let subel = get_subelement qname el in
          Some (get_cdata subel)
    with Not_found -> None

  let encode_muc ?maxchars ?maxstanzas ?seconds ?since ?password () =
    let history_attrs =
      List.fold_left (fun acc (k,v) ->
        match v with
          | None -> acc
          | Some i -> make_attr k (string_of_int i) :: acc
      ) [] ["maxchars", maxchars;
            "maxstanzas", maxstanzas;
            "seconds", seconds;
            "since", since] in
    let history =
      if history_attrs <> [] then
        [make_element (ns_muc, "history") history_attrs []]
      else
        []
    in
    let els =
      match password with
        | None -> history
        | Some v -> make_simple_cdata (ns_muc, "password") v :: history
    in
      make_element (ns_muc, "x") [] els

  type muc_data = {
    maxchars : int option;
    maxstanzas : int option;
    seconds : int option;
    since : int option;
    password : string option
  }
    
  let try_int_of_string i =
    try Some (int_of_string i) with _ -> None
    
  let decode_muc el =
    let password = get_subcdata_option (ns_muc, "password") el in
    let history =
      try Some (get_subelement (ns_muc, "history") el) with Not_found -> None in
      match history with
        | None -> {maxchars = None;
                   maxstanzas = None;
                   seconds = None;
                   since = None;
                   password = password}
        | Some el ->
          let maxchars, maxstanzas, seconds, since =
            List.fold_left
              (fun (maxchars, maxstanzas, seconds, since) (k,v) ->
                match k with
                  | None, "maxchars" ->
                    (try_int_of_string v, maxstanzas, seconds, since)
                  | None, "maxstanzas" ->
                    (maxchars, try_int_of_string v, seconds, since)
                  | None, "seconds" ->
                    (maxchars, maxstanzas, try_int_of_string v, since)
                  | None, "since" ->
                    (maxchars, maxstanzas, seconds, try_int_of_string v)
                  | _ ->
                    (maxchars, maxstanzas, seconds, since)
              ) (None, None, None, None) (get_attrs el)
          in
            {maxchars = maxchars;
             maxstanzas = maxstanzas;
             seconds = seconds;
             since = since;
             password = password}
              
  module User =
  struct

    type item = {
      actor : JID.t option;
      continue : string option;
      reason : string option;
      jid : JID.t option;
      nick : string option;
      affiliation : affiliation option;
      role : role option
    }
    
    type  data = {
      decline : (JID.t option * JID.t option * string option) option;
      destroy : (JID.t option * string option) option;
      invite : (JID.t option * JID.t option * string option) list;
      item : item option;
      password : string option;
      status : int list
    }
        
    let encode_decline ?jid_from ?jid_to ?reason () =
      make_element (ns_muc_user, "decline")
        (List.fold_left (fun acc (k,v) ->
          match v with
            | None -> acc
            | Some v -> make_attr k v :: acc
         ) [] ["from", maybe string_of_jid jid_from;
               "to", maybe string_of_jid jid_to])
        (match reason with
          | None -> []
          | Some v -> [make_simple_cdata (ns_muc_user, "reason") v])

    let decode_decline el =
      let attrs = get_attrs el in
      let jid_from = maybe JID.of_string (get_attr_value_option "from" attrs) in
      let jid_to = maybe JID.of_string (get_attr_value_option "to" attrs) in
      let reason = get_subcdata_option (ns_muc_user, "reason") el in
        (jid_from, jid_to, reason)
  
    let encode_destroy ?jid ?reason () =
      make_element (ns_muc_user, "destroy")
        (match jid with None -> [] | Some v ->
          [make_attr "jid" (string_of_jid v)])
        (match reason with
          | None -> []
          | Some v -> [make_simple_cdata (ns_muc_user, "reason") v])

    let decode_destroy el =
      let jid = maybe JID.of_string
        (get_attr_value_option "jid" (get_attrs el)) in
      let reason = get_subcdata_option (ns_muc_user, "reason") el in
        (jid, reason)
          
    let encode_invite ?jid_from ?jid_to ?reason () =
      make_element (ns_muc_user, "invite")
        (List.fold_left (fun acc (k,v) ->
          match v with
            | None -> acc
            | Some v -> make_attr k v :: acc
         ) [] ["from", maybe string_of_jid jid_from;
               "to", maybe string_of_jid jid_to])
        (match reason with
          | None -> []
          | Some v -> [make_simple_cdata (ns_muc_user, "reason") v])
        
    let decode_invite el =
      let reason = get_subcdata_option (ns_muc_user, "reason") el in
      let attrs = get_attrs el in
      let jid_from = maybe JID.of_string (get_attr_value_option "from" attrs) in
      let jid_to = maybe JID.of_string (get_attr_value_option "to" attrs) in
        (jid_from, jid_to, reason)
  
    let encode_item ?actor ?reason ?continue ?affiliation ?jid ?nick ?role () =
      make_element (ns_muc_user, "item")
        (List.fold_left (fun acc (k,v) ->
          match v with
            | None -> acc
            | Some v -> make_attr k v :: acc
         ) []
           ["jid", maybe string_of_jid jid;
            "nick", nick;
            "affiliation", (match affiliation with
              | None -> None
              | Some AffiliationOwner -> Some "owner"
              | Some AffiliationAdmin -> Some "admin"
              | Some AffiliationMember -> Some "member"
              | Some AffiliationOutcast -> Some "outcast"
              | Some AffiliationNone -> Some "none");
            "role", (match role with
              | None -> None
              | Some RoleModerator -> Some "moderator"
              | Some RoleParticipant -> Some "participant"
              | Some RoleVisitor -> Some "visitor"
              | Some RoleNone -> Some "none")])
        (List.fold_left (fun acc -> function
          | None -> acc
          | Some el -> el :: acc
         ) []
           [(match actor with
             | None -> None
             | Some v ->
               Some (make_element (ns_muc_user, "actor")
                       [make_attr "jid" (string_of_jid v)] [])
            );
            (match reason with
              | None -> None
              | Some v -> Some (make_simple_cdata (ns_muc_user, "reason") v));
            (match continue with
              | None -> None
              | Some thread ->  (* TODO: thread is optional *)
                Some (make_element (ns_muc_user, "continue")
                        [make_attr "thread" thread] []))
           ])
        
    let decode_item el =
      let actor =
        try let subel = get_subelement (ns_muc_user, "actor") el in
              Some (JID.of_string (get_attr_value "jid" (get_attrs subel)))
        with Not_found -> None in
      let continue =
        try let subel = get_subelement (ns_muc_user, "continue") el in
              Some (get_attr_value "thread" (get_attrs subel))
        with Not_found -> None in
      let reason = get_subcdata_option (ns_muc_user, "reason") el in
      let attrs = get_attrs el in
      let affiliation =
        match get_attr_value_option "affiliation" attrs with
          | None -> None
          | Some "owner" -> Some AffiliationOwner
          | Some "admin" -> Some AffiliationAdmin
          | Some "member" -> Some AffiliationMember
          | Some "outcast" -> Some AffiliationOutcast
          | Some "none" -> Some AffiliationNone
          | Some _ -> Some AffiliationNone
      in
      let jid = maybe JID.of_string (get_attr_value_option "jid" attrs) in
      let nick = get_attr_value_option "nick" attrs in
      let role =
        match get_attr_value_option "role" attrs with
          | None -> None
          | Some "moderator" -> Some RoleModerator
          | Some "participant" -> Some RoleParticipant
          | Some "visitor" -> Some RoleVisitor
          | Some "none" -> Some RoleNone
          | Some _ -> Some RoleNone
      in
        { actor = actor;
          continue = continue;
          reason = reason;
          jid = jid;
          nick = nick;
          affiliation = affiliation;
          role = role
        }
          
    let encode_password password =
      make_simple_cdata (ns_muc_user, "password") password
        
    let decode_password el =
      get_cdata el
        
    let encode_status code =
      make_element (ns_muc_user, "status")
        [make_attr "code" (string_of_int code)] []
        
    let decode_status el =
      let code = get_attr_value "code" (get_attrs el) in
        int_of_string code
          
    let encode data =
      let els =
        List.fold_left (fun acc -> function
          | None -> acc
          | Some el -> el :: acc
        )
          (List.map (fun (jid_from, jid_to, reason) ->
            encode_invite ?jid_from ?jid_to ?reason ()) data.invite)
          (maybe
             (fun (jid_from, jid_to, reason) ->
               encode_decline ?jid_from ?jid_to ?reason ()) data.decline ::
             maybe
             (fun (jid, reason) -> encode_destroy ?jid ?reason ()) data.destroy ::
             maybe
             (fun i ->
               encode_item ?actor:i.actor ?reason:i.reason ?continue:i.continue
                 ?affiliation:i.affiliation ?jid:i.jid ?nick:i.nick
                 ?role:i.role ()) data.item ::
             maybe encode_password data.password ::
             (List.map (fun code -> Some (encode_status code)) data.status))
      in         
        make_element (ns_muc_user, "x") [] els
          
    let decode el =
      let data =
        List.fold_left
          (fun data -> function
            | Xmlelement ((ns, name), _, _) as el
                when ns = ns_muc_user -> (
                  match name with
                    | "decline" -> (
                      match data.decline with
                        | None ->
                          let jid_from, jid_to, reason = decode_decline el in
                            {data with
                              decline = Some (jid_from, jid_to, reason) }
                        | Some _ ->
                          data
                    )
                    | "destriy" -> (
                      match data.destroy with
                        |None ->
                          let jid, reason = decode_destroy el in
                            {data with destroy = Some (jid, reason) }
                        | Some _ ->
                          data
                    )
                    | "invite" ->
                      let jid_from, jid_to, reason = decode_invite el in
                        {data with
                          invite = (jid_from, jid_to, reason) :: data.invite}
                    | "item" -> (
                      match data.item with
                        | None ->
                          let i = decode_item el in
                            {data with item = Some i}
                        | Some _ ->
                          data
                    )
                    | "password" -> (
                      match data.password with
                        | None ->
                          let p = decode_password el in
                            {data with password = Some p}
                        | Some _ ->
                          data
                    )
                    | "status" ->
                      let s = decode_status el in
                        {data with status = s :: data.status}
                    | _ -> (* unknown *)
                      data
                )
            | Xmlelement _
            | Xmlcdata _ ->
              data
          ) {decline = None;
             destroy = None;
             invite = [];
             item = None;
             password = None;
             status = []} (get_children el) in
        data
          
  end

  module Admin =
  struct
    let encode items =
      make_element (ns_muc_admin, "query") [] items
        
    let encode_item  ?actor ?reason ?affiliation ?jid ?nick ?role () =
      make_element (ns_muc_admin, "item")
        (List.fold_left (fun acc (k,v) ->
          match v with
            | None -> acc
            | Some v -> make_attr k v :: acc
         ) []
           ["jid", jid;
            "nick", nick;
            "affiliation", (match affiliation with
              | None -> None
              | Some AffiliationOwner -> Some "owner"
              | Some AffiliationAdmin -> Some "admin"
              | Some AffiliationMember -> Some "member"
              | Some AffiliationOutcast -> Some "outcast"
              | Some AffiliationNone -> Some "none");
            "role", (match role with
              | None -> None
              | Some RoleModerator -> Some "moderator"
              | Some RoleParticipant -> Some "participant"
              | Some RoleVisitor -> Some "visitor"
              | Some RoleNone -> Some "none")])
        (List.fold_left (fun acc -> function
          | None -> acc
          | Some el -> el :: acc
         ) []
           [(match actor with
             | None -> None
             | Some v ->
               Some (make_element (ns_muc_admin, "actor")
                       [make_attr "jid" (string_of_jid v)] [])
            );
            (match reason with
              | None -> None
              | Some v -> Some (make_simple_cdata (ns_muc_admin, "reason") v))
           ])
        
    type item = {
      actor : JID.t option;
      reason : string option;
      jid : JID.t option;
      nick : string option;
      affiliation : affiliation option;
      role : role option
    }
        
    let decode_item el =
      let actor =
        try let subel = get_subelement (ns_muc_admin, "actor") el in
              Some (JID.of_string (get_attr_value "jid" (get_attrs subel)))
        with Not_found -> None in
      let reason = get_subcdata_option (ns_muc_admin, "reason") el in
      let attrs = get_attrs el in
      let affiliation =
        match get_attr_value_option "affiliation" attrs with
          | None -> None
          | Some "owner" -> Some AffiliationOwner
          | Some "admin" -> Some AffiliationAdmin
          | Some "member" -> Some AffiliationMember
          | Some "outcast" -> Some AffiliationOutcast
          | Some "none" -> Some AffiliationNone
          | Some _ -> Some AffiliationNone
      in
      let jid = maybe JID.of_string (get_attr_value_option "jid" attrs) in
      let nick = get_attr_value_option "nick" attrs in
      let role =
        match get_attr_value_option "role" attrs with
          | None -> None
          | Some "moderator" -> Some RoleModerator
          | Some "participant" -> Some RoleParticipant
          | Some "visitor" -> Some RoleVisitor
          | Some "none" -> Some RoleNone
          | Some _ -> Some RoleNone
      in
        { actor = actor;
          reason = reason;
          jid = jid;
          nick = nick;
          affiliation = affiliation;
          role = role}
          
    let decode el =
      let items = get_subelements (ns_muc_admin, "item") el in
        List.map decode_item items
  end
    
  module Owner =
  struct
    let encode_destroy ?jid ?password ?reason () =
      make_element (ns_muc_owner, "query") []
        [make_element (ns_muc_owner, "destroy")
            (match jid with None -> [] | Some v -> [make_attr "jid" v])
            (List.fold_left (fun acc -> function
              | None -> acc
              | Some el -> el :: acc
             ) []
               [(match password with
                 | None -> None
                 | Some v ->
                   Some (make_simple_cdata (ns_muc_owner, "password") v));
                (match reason with
                  | None -> None
                  | Some v ->
                    Some (make_simple_cdata (ns_muc_owner, "reason") v))]
            )]
        
    let decode_destroy el =
      let el = get_subelement (ns_muc_owner, "destroy") el in
      let jid = get_attr_value_option "jid" (get_attrs el) in
      let password = get_subcdata_option (ns_muc_owner, "password") el in
      let reason = get_subcdata_option (ns_muc_owner, "reason") el in
        (jid, password, reason)
          
    let encode_xdata xdata =
      make_element (ns_muc_owner, "query") [] [xdata]
        
    let decode_xdata el =
      let module Xdata = XEP_xdata.Make(X) in
      let x = get_subelement (Xdata.ns_xdata, "x") el in
        Xdata.decode x
  end
    
  module Unique =
  struct
    let encode () =
      make_element (ns_muc_unique, "unique") [] []
        
    let decode _el =
      ()
  end
    
  let enter_room xmpp ?maxchars ?maxstanzas ?seconds ?since ?password
      ?nick room =
    let nick =
      match nick with
        | None ->
          (get_myjid xmpp).node
        | Some v -> v
    in
      send_presence xmpp ~jid_to:(replace_resource room nick)
        ~x:[encode_muc ?maxchars ?maxstanzas ?seconds ?since ?password ()] ()
        
  let leave_room xmpp ?reason ~nick room =
    send_presence xmpp ~jid_to:(replace_resource room nick)
      ~kind:Unavailable ?status:reason ()
      
end
