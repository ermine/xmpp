(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *
 * XEP-0054: vcard-temp
 * Version: 1.2
 *)

module Make (X : XMPP.S) =
struct
  open Xml
  
  let ns_vcard = Some "vcard-temp"

  type n = {
    family : string;
    given : string;
    middle : string;
    prefix : string;
    suffix : string
  }
 
  type image =
    | Binval of string * string
    | Extval of string
        
  type dst_t =
    | DOM
    | INTL

  type adr = {
    adr_home : bool;
    adr_work : bool;
    adr_postal : bool;
    adr_parcel: bool;
    adr_dst : dst_t option;
    adr_pref : bool;
    pobox : string;
    extadd : string;
    street : string;
    locality : string;
    region : string;
    pcode : string;
    ctry : string
  }
      
  type label = {
    label_home : bool;
    label_work : bool;
    label_postal : bool;
    label_parcel : bool;
    label_dst : dst_t option;
    label_pref : bool;
    line : string list;
  }
      
  type tel = {
    tel_home : bool;
    tel_work : bool;
    tel_voice : bool;
    tel_fax : bool;
    tel_pager : bool;
    tel_msg : bool;
    tel_cell : bool;
    tel_video : bool;
    tel_bbs : bool;
    tel_modem : bool;
    tel_isdn : bool;
    tel_pcs : bool;
    tel_pref : bool;
    number : string
  }
      
  type email = {
    email_home : bool;
    email_work : bool;
    email_internet : bool;
    email_pref : bool;
    email_x400 : bool;
    userid : string
  }
      
  type class_t =
    | Public
    | Private
    | Confidential
        
  type geo = {
    lat : string;
    lon : string
  }
      
  type org = {
    orgname : string;
    orgunit : string
  }
      
  type sound =
    | Phonetic of string
    | SoundBinval of string
    | SoundExtval of string
        
  type field =
    | Nickname of string
    | Photo of image
    | BDay of string
    | Adr of adr
    | Label of label
    | Tel of tel
    | Email of email
    | JabberID of string
    | Mailer of string
    | TZ of string
    | GEO of geo
    | Title of string
    | Role of string
    | Logo of image
    | Agent of agent_t
    | Org of org
    | Categories of string list
    | Note of string
    | ProdID of string
    | REV of string
    | SortString of string
    | Sound of sound
    | UID of string
    | URL of string
    | Class of class_t
    | Key of string * string
    | Desc of string
  and agent_t =
    | VCard of vcard
    | AgentExtval of string
  and vcard = {
    fn : string;
    n : n;
    fields : field list
  }
      
      
  let encode_n item =
    let els =
      List.fold_left (fun acc (field, value) ->
        if value = "" then
          acc
        else
          make_simple_cdata (ns_vcard, field) value :: acc
      ) [] ["FAMILY", item.family;
            "GIVEN", item.given;
            "MIDDLE", item.middle;
            "PREFIX", item.prefix;
            "SUFFIX", item.suffix] in
      make_element (ns_vcard, "N") [] els
        
  let encode_adr item =
    make_element (ns_vcard, "ADR") []
      (List.fold_left (fun acc -> function
        | None -> acc
        | Some (k,v) -> make_simple_cdata (ns_vcard, k) v :: acc
       ) []
         [(if item.adr_home = true then Some ("HOME", "") else None);
          (if item.adr_work = true then Some ("WORK", "") else None);
          (if item.adr_postal = true then Some ("POSTAL", "") else None);
          (if item.adr_parcel = true then Some ("PARCEL", "") else None);
          (match item.adr_dst with
            | None -> None
            | Some DOM -> Some ("DOM", "")
            | Some INTL -> (Some ("INTL", "")));
          (if item.adr_pref = true then Some ("PREF", "") else None);
          (if item.pobox <> "" then Some ("POBOX", item.pobox) else None);
          (if item.extadd <> "" then Some ("EXTADD", item.extadd) else None);
          (if item.street <> "" then Some ("STREET", item.street) else None);
          (if item.locality <> "" then Some ("LOCALITY", item.locality) else None);
          (if item.region <> "" then Some ("REGION", item.region) else None);
          (if item.pcode <> "" then Some ("PCODE", item.pcode) else None);
          (if item.ctry <> "" then Some ("CTRY", item.ctry) else None)
         ])
      
  let encode_label item =
    make_element (ns_vcard, "LABEL") []
      (List.fold_left (fun acc -> function
        | None -> acc
        | Some (k,v) -> make_simple_cdata (ns_vcard, k) v :: acc
       )
         (List.map (make_simple_cdata (ns_vcard, "LINE")) item.line)
         [(if item.label_home = true then Some ("HOME", "") else None);
          (if item.label_work = true then Some ("WORK", "") else None);
          (if item.label_postal = true then Some ("POSTAL", "") else None);
          (if item.label_parcel = true then Some ("PARCEL", "") else None);
          (match item.label_dst with
            | None -> None
            | Some DOM -> Some ("DOM", "")
            | Some INTL -> (Some ("INTL", "")));
          (if item.label_pref = true then Some ("PREF", "") else None);
         ])
      
  let encode_tel item =
    make_element (ns_vcard, "TEL") []
      (List.fold_left (fun acc -> function
        | None -> acc
        | Some (k,v) -> make_simple_cdata (ns_vcard, k) v :: acc
       ) []
         [(if item.tel_home = true then Some ("HOME", "") else None);
          (if item.tel_work = true then Some ("WORK", "") else None);
          (if item.tel_voice = true then Some ("VOICE", "") else None);
          (if item.tel_fax = true then Some ("FAX", "") else None);
          (if item.tel_pager = true then Some ("PAGER", "") else None);
          (if item.tel_msg = true then Some ("MSG", "") else None);
          (if item.tel_cell = true then Some ("CELL", "") else None);
          (if item.tel_video = true then Some ("VIDEO", "") else None);
          (if item.tel_bbs = true then Some ("BBS", "") else None);
          (if item.tel_modem = true then Some ("MODEM", "") else None);
          (if item.tel_isdn = true then Some ("ISDN", "") else None);
          (if item.tel_pcs = true then Some ("PCS", "") else None);
          (if item.tel_pref = true then Some ("PREF", "") else None);
          Some ("NUMBER", item.number)
         ])
      
  let encode_email item =
    make_element (ns_vcard, "EMAIL") []
      (List.fold_left (fun acc -> function
        | None -> acc
        | Some (k,v) -> make_simple_cdata (ns_vcard, k) v :: acc
       ) []
         [(if item.email_home = true then Some ("HOME", "") else None);
          (if item.email_work = true then Some ("WORK", "") else None);
          (if item.email_internet = true then Some ("INTERNET", "") else None);
          (if item.email_pref = true then Some ("Pref", "") else None);
          (if item.email_x400 = true then Some ("X400", "") else None);
          Some ("USERID", item.userid)
         ])
      
  let rec encode vcard  =
    let encode_fields fields =
      List.map (function
        | Nickname nick ->
          make_simple_cdata (ns_vcard, "NICKNAME") nick
        | Photo image ->
          make_element (ns_vcard, "PHOTO") []
            (match image with
              | Binval (type_, str) ->
                [make_simple_cdata (ns_vcard, "TYPE") type_;
                 make_simple_cdata (ns_vcard, "BINVAL") str]
              | Extval str ->
                [make_simple_cdata (ns_vcard, "EXTVAL") str]
            )
        | BDay bday ->
          make_simple_cdata (ns_vcard, "BDAY") bday
        | Adr adr ->
          encode_adr adr
        | Label label ->
          encode_label label
        | Tel tel ->
          encode_tel tel
        | Email email ->
          encode_email email
        | JabberID jid ->
          make_simple_cdata (ns_vcard, "JABBERID") jid
        | Mailer mailer ->
          make_simple_cdata (ns_vcard, "MAILER") mailer
        | TZ tz ->
          make_simple_cdata (ns_vcard, "TZ") tz
        | GEO geo ->
          make_element (ns_vcard, "GEO") []
            [make_simple_cdata (ns_vcard, "LAT") geo.lat;
             make_simple_cdata (ns_vcard, "LON") geo.lon;
            ]
        | Title title ->
          make_simple_cdata (ns_vcard, "TITLE") title
        | Role role ->
          make_simple_cdata (ns_vcard, "ROLE") role
        | Logo image ->
          make_element (ns_vcard, "LOGO") []
            (match image with
              | Binval (type_, str) ->
                [make_simple_cdata (ns_vcard, "TYPE") type_;
                 make_simple_cdata (ns_vcard, "BINVAL") str]
              | Extval str ->
                [make_simple_cdata (ns_vcard, "EXTVAL") str]
            )
        | Agent agent -> (
          match agent with
            | VCard vcard ->
              encode vcard
            | AgentExtval str ->
              make_simple_cdata (ns_vcard, "EXTVAL") str
        )
        | Org org ->
          make_element (ns_vcard, "ORG") []
            [make_simple_cdata (ns_vcard, "ORGNAME") org.orgname;
             make_simple_cdata (ns_vcard, "ORGUNIT") org.orgunit]
        | Categories c ->
          make_element (ns_vcard, "CATEGORIES") []
            (List.map (make_simple_cdata (ns_vcard, "KEYWORD")) c)
        | Note note ->
          make_simple_cdata (ns_vcard, "NOTE") note
        | ProdID prodid ->
          make_simple_cdata (ns_vcard, "PRODID") prodid
        | REV rev ->
          make_simple_cdata (ns_vcard, "REV") rev
        | SortString str ->
          make_simple_cdata (ns_vcard, "SORT-STRING") str
        | Sound sound ->
          let el =
            match sound with
              | Phonetic v ->
                make_simple_cdata (ns_vcard, "PHONETIC") v
              | SoundBinval v ->
                make_simple_cdata (ns_vcard, "BINVAL") v
              | SoundExtval v ->
                make_simple_cdata (ns_vcard, "EXTVAL") v
          in
            make_element (ns_vcard, "SOUND") [] [el]
        | UID uid ->
          make_simple_cdata (ns_vcard, "UID") uid
        | URL url ->
          make_simple_cdata (ns_vcard, "URL") url
        | Class c ->
          let tag =
            match c with
              | Public -> "PUBLIC"
              | Private -> "PRIVATE"
              | Confidential -> "CONFIDENTIAL"
          in
            make_element (ns_vcard, "CLASS") []
              [make_simple_cdata (ns_vcard, tag) ""]
        | Key (type_, cred) ->
          make_element (ns_vcard, "KEY") []
            (if type_ <> "" then
                [make_simple_cdata (ns_vcard, "TYPE") type_;
                 make_simple_cdata (ns_vcard, "CRED") cred]
             else
                [make_simple_cdata (ns_vcard, "CRED") cred]
            )
        | Desc desc ->
          make_simple_cdata (ns_vcard, "DESC") desc
      ) fields
    in
      make_element (ns_vcard, "vCard") []
        ((make_simple_cdata (ns_vcard, "FN") vcard.fn) ::
            (encode_n vcard.n) ::
            (encode_fields vcard.fields))
        
  let decode_categories els =
    List.fold_left (fun acc -> function
      | Xmlelement (qname, _, els) ->
        if qname = (ns_vcard, "KEYWORD") then
          collect_cdata els :: acc
        else
          acc
      | Xmlcdata _ ->
        acc
    ) [] els
      
      
  let decode_n els =
    List.fold_left (fun item -> function
      | Xmlelement (qname, _, els)
          when get_namespace qname = ns_vcard -> (
            match get_name qname with
              | "FAMILY" ->
                {item with family = collect_cdata els}
              | "GIVEN" ->
                {item with given = collect_cdata els}
              | "MIDDLE" ->
                {item with middle = collect_cdata els}
              |  "PREFIX" ->
                {item with prefix = collect_cdata els}
              | "SUFFIX" ->
                {item with suffix = collect_cdata els}
              | _ ->
                item
          )
      | Xmlelement _
      | Xmlcdata _ ->
        item
    ) {family = "";
       given = "";
       middle = "";
       prefix = "";
       suffix = ""
      } els
      
  let decode_adr els =
    let item = 
      List.fold_left (fun item -> function
        | Xmlelement (qname, _, _) when
            get_namespace qname = ns_vcard -> (
              match get_name qname with
                | "HOME" ->
                  {item with adr_home = true}
                | "WORK" ->
                  {item with adr_work = true}
                | "POSTAL" ->
                  {item with adr_postal = true}
                | "PARCEL" ->
                  {item with adr_parcel = true}
                | "DOM" ->
                  {item with adr_dst = Some DOM}
                | "INTL" ->
                  {item with adr_dst = Some INTL}
                | "PREF" ->
                  {item with adr_pref = true}
                | "POBOX" ->
                  let text = collect_cdata els in
                    if text = "" then
                      item
                    else
                      {item with pobox = text}
                | "EXTADD" ->
                  let text = collect_cdata els in
                    if text = "" then
                      item
                    else
                      {item with extadd = text}
                | "STREET" ->
                  let text = collect_cdata els in
                    if text = "" then
                      item
                    else
                      {item with street = text}
                | "LOCALITY" ->
                  let text = collect_cdata els in
                    if text = "" then
                      item
                    else
                      {item with locality = text}
                | "REGION" ->
                  let text = collect_cdata els in
                    if text = "" then
                      item
                    else
                      {item with region = text}
                | "PCODE" ->
                  let text = collect_cdata els in
                    if text = "" then
                      item
                    else
                      {item with pcode = text}
                | "CTRY" ->
                  let text = collect_cdata els in
                    if text = "" then
                      item
                    else
                      {item with ctry = text}
                | _ ->
                  item
            )
        | Xmlelement _
        | Xmlcdata _ ->
          item
      ) { adr_home = false;
          adr_work = false;
          adr_postal = false;
          adr_parcel= false;
          adr_dst = None;
          adr_pref = false;
          pobox = "";
          extadd = "";
          street = "";
          locality = "";
          region = "";
          pcode = "";
          ctry = "";
        } els in
      if item.pobox = "" && item.extadd = "" && item.street = "" &&
        item.locality = "" && item.region = "" && item.pcode = "" &&
        item.ctry = "" then
        None
      else
        Some item
          
  let decode_label els =
    let item =
      List.fold_left (fun item -> function
        | Xmlelement (qname, _, _) when
            get_namespace qname = ns_vcard -> (
              match get_name qname with
                | "HOME" ->
                  {item with label_home = true}
                | "WORK" ->
                  {item with label_work = true}
                | "POSTAL" ->
                  {item with label_postal = true}
                | "PARCEL" ->
                  {item with label_parcel = true}
                | "DOM" ->
                  {item with label_dst = Some DOM}
                | "INTL" ->
                  {item with label_dst = Some INTL}
                | "PREF" ->
                  {item with label_pref = true}
                | "LINE" ->
                  let text = collect_cdata els in
                    if text = "" then
                      item
                    else
                      {item with line = text :: item.line}
                | _ ->
                  item
            )
        | Xmlelement _
        | Xmlcdata _ ->
          item
      ) { label_home = false;
          label_work = false;
          label_postal = false;
          label_parcel = false;
          label_dst = None;
          label_pref = false;
          line = []
        } els in
      if item.line = [] then
        None
      else
        Some item
          
  let decode_tel els =
    let item =
      List.fold_left (fun item -> function
        | Xmlelement (qname, _, _) when
            get_namespace qname = ns_vcard -> (
              match get_name qname with
                | "HOME" ->
                  {item with tel_home = true}
                | "WORK" ->
                  {item with tel_work = true}
                | "VOICE" ->
                  {item with tel_voice = true}
                | "FAX" ->
                  {item with tel_fax = true}
                | "PAGER" ->
                  {item with tel_pager = true}
                | "MSG" ->
                  {item with tel_msg = true}
                | "CELL" ->
                  {item with tel_cell = true}
                | "VIDEO" ->
                  {item with tel_video = true}
                | "BBS" ->
                  {item with tel_bbs = true}
                | "MODEM" ->
                  {item with tel_modem = true}
                | "ISDN" ->
                  {item with tel_isdn = true}
                | "PCS" ->
                  {item with tel_pcs = true}
                | "PREF" ->
                  {item with tel_pref = true}
                | "NUMBER" ->
                  let text = collect_cdata els in
                    if text = "" then
                      item
                    else
                      {item with number = text}
                | _ ->
                  item
            )
        | Xmlelement _
        | Xmlcdata _ ->
          item
      ) { tel_home = false;
          tel_work = false;
          tel_voice = false;
          tel_fax = false;
          tel_pager = false;
          tel_msg = false;
          tel_cell = false;
          tel_video =false;
          tel_bbs = false;
          tel_modem = false;
          tel_isdn = false;
          tel_pcs = false;
          tel_pref = false;
          number = ""
        } els in
      if item.number = "" then
        None
      else
        Some item
          
  let decode_email els =
    let item =
      List.fold_left (fun item -> function
        | Xmlelement (qname, _, _) when
            get_namespace qname = ns_vcard -> (
              match get_name qname with
                | "HOME" ->
                  {item with email_home = true}
                | "WORK" ->
                  {item with email_work = true}
                | "INTERNET" ->
                  {item with email_internet = true}
                | "PREF" ->
                  {item with email_pref = true}
                | "X400" ->
                  {item with email_x400 = true}
                | "USERID" ->
                  let text = collect_cdata els in
                    if text ="" then
                      item
                    else
                      {item with userid = text}
                | _ ->
                  item
            )
        | Xmlelement _
        | Xmlcdata _ ->
          item
      ) { email_home = false;
          email_work = false;
          email_internet = false;
          email_pref = false;
          email_x400 = false;
          userid = ""
        } els in
      if item.userid = "" then
        None
      else
        Some item
          
  let decode_image els =
    try
      let binval = get_cdata (get_element (ns_vcard, "BINVAL") els) in
      let type_ = get_cdata (get_element (ns_vcard, "TYPE") els) in
        Some (Binval (type_, binval))
    with Not_found ->
      try
        let extval = get_cdata (get_element (ns_vcard, "EXTVAL") els) in
          Some (Extval extval)
      with Not_found -> None
        
  let decode_geo els =
    try
      let lat = get_cdata (get_element (ns_vcard, "LAT") els) in
      let lon = get_cdata (get_element (ns_vcard, "LON") els) in
        Some {lat = lat; lon = lon}
    with Not_found -> None
      
  let decode_org els =
    try
      let orgname = get_cdata (get_element (ns_vcard, "ORGNAME") els) in
      let orgunit = get_cdata (get_element (ns_vcard, "ORGUNIT") els) in
        Some {orgname = orgname; orgunit = orgunit}
    with Not_found -> None
      
  let decode_key els =
    try
      let cred = get_cdata (get_element (ns_vcard, "CRED") els) in
      let type_ =
        try get_cdata (get_element (ns_vcard, "TYPE") els)
        with Not_found -> "" in
        Some (type_, cred)
    with Not_found -> None
      
  let decode_class els =
    let rec aux_fold = function
      | [] -> None
      | el :: tail ->
        match el with
          | Xmlelement (qname, _, _) ->
            if qname = (ns_vcard, "PUBLIC") then
              Some Public
            else if qname = (ns_vcard, "PRIVATE") then
              Some Private
            else if qname = (ns_vcard, "CONFIDENTIAL") then
              Some Confidential
            else
              aux_fold tail
          | Xmlcdata _ ->
            aux_fold tail
    in
      aux_fold els
        
  let decode_sound els =
    let rec aux_fold = function
      | [] -> None
      | el :: tail ->
        match el with
          | Xmlelement (qname, _, els) ->
            if qname = (ns_vcard, "PHONETIC") then
              Some (Phonetic (collect_cdata els))
            else if qname = (ns_vcard, "BINVAL") then
              Some (SoundBinval (collect_cdata els))
            else if qname = (ns_vcard, "EXTVAL") then
              Some (SoundExtval (collect_cdata els))
            else
              aux_fold tail
          | Xmlcdata _ ->
            aux_fold tail
    in
      aux_fold els
        
        
  let rec decode el =
    let (fn, n, fields) =
      List.fold_left (fun (fn, n, fields) -> function
        | Xmlelement (qname, _, els) when
            get_namespace qname = ns_vcard -> (
              match get_name qname with
                | "VERSION" -> (* must be "2.0" *)
                  (fn, n, fields)
                | "FN" ->
                  if fn = "" then
                    ((collect_cdata els), n, fields)
                  else
                    (fn, n, fields)
                | "N" -> (
                  match n with
                    | None -> (fn, Some (decode_n els), fields)
                    | Some _ -> (fn, n, fields)
                )
                | "NICKNAME" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, Nickname text :: fields)
                    else
                      (fn, n, fields)
                | "PHOTO" -> (
                  match decode_image els with
                    | None -> (fn, n, fields)
                    | Some img -> (fn, n, Photo img :: fields)
                )
                | "BDAY" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, BDay text :: fields)
                    else
                      (fn, n, fields)
                | "ADR" -> (
                  match decode_adr els with
                    | None -> (fn, n, fields)
                    | Some adr -> (fn, n, Adr adr :: fields)
                )
                | "LABEL" -> (
                  match decode_label els with
                    | None -> (fn, n, fields)
                    | Some l -> (fn, n, Label l :: fields)
                )
                | "TEL" -> (
                  match decode_tel els with
                    | None -> (fn, n, fields)
                    | Some tel -> (fn, n, Tel tel :: fields)
                )
                | "EMAIL" -> (
                  match decode_email els with
                    | None -> (fn, n, fields)
                    | Some email -> (fn, n, Email email :: fields)
                )
                | "JABBERID" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, JabberID text :: fields)
                    else
                      (fn, n, fields)
                | "MAILER" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, Mailer text :: fields)
                    else
                      (fn, n, fields)
                | "TZ" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, TZ text :: fields)
                    else
                      (fn, n, fields)
                | "GEO" -> (
                  match decode_geo els with
                    | None -> (fn, n, fields)
                    | Some g -> (fn, n, GEO g :: fields)
                )
                | "TITLE" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, Title text :: fields)
                    else
                      (fn, n, fields)
                | "ROLE" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, Role text :: fields)
                    else
                      (fn, n, fields)                              
                | "LOGO" -> (
                  match decode_image els with
                    | None -> (fn, n, fields)
                    | Some img -> (fn, n, Logo img :: fields)
                )
                | "AGENT" -> (
                  match decode_agent els with
                    | None -> (fn, n, fields)
                    | Some agent -> (fn, n, Agent agent :: fields)
                )
                | "ORG" -> (
                  match decode_org els with
                    | None -> (fn, n, fields)
                    | Some o -> (fn, n, Org o :: fields)
                )
                | "CATEGORIES" -> (
                  match decode_categories els with
                    | [] -> (fn, n, fields)
                    | xs -> (fn, n, Categories xs :: fields)
                )
                | "NOTE" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, Note text :: fields)
                    else
                      (fn, n, fields)
                | "PRODID" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, ProdID text :: fields)
                    else
                      (fn, n, fields)
                | "REV" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, REV text :: fields)
                    else
                      (fn, n, fields)
                | "SORT-STRING" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, SortString text :: fields)
                    else
                      (fn, n, fields)
                | "SOUND" -> (
                  match decode_sound els with
                    | None -> (fn, n, fields)
                    | Some s -> (fn, n, Sound s :: fields)
                )
                | "UID" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, UID text :: fields)
                    else
                      (fn, n, fields)
                | "URL" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, URL text :: fields)
                    else
                      (fn, n, fields)
                | "CLASS" -> (
                  match decode_class els with
                    | None -> (fn, n, fields)
                    | Some c -> (fn, n, Class c :: fields)
                )
                | "KEY" -> (
                  match decode_key els with
                    | None -> (fn, n, fields)
                    | Some (type_, cred) ->
                      (fn, n, Key (type_, cred) :: fields)
                )
                | "DESC" ->
                  let text = collect_cdata els in
                    if text <> "" then
                      (fn, n, Desc text :: fields)
                    else
                      (fn, n, fields)
                | _ ->
                  (fn, n, fields)
            )
        | Xmlelement _
        | Xmlcdata _ ->
          (fn, n, fields)
      ) ("", None, []) (get_children el)
    in
      {fn = fn;
       n = (match n with
         | None -> {family = ""; given = ""; middle = "";
                    prefix = ""; suffix =""}
         | Some v -> v);
       fields = fields}
  and decode_agent els =
    try
      let extval = get_cdata (get_element (ns_vcard, "EXTVAL") els) in
        if extval = "" then
          None
        else
          Some (AgentExtval extval)
    with Not_found ->
      try
        let vcard = get_element (ns_vcard, "vCard") els in
          Some (VCard (decode vcard))
      with Not_found -> None
        
  let make_iq_get () =
    make_element (ns_vcard, "vCard") [] []
end
