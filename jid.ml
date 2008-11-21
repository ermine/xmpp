(*
 * (c) 2004-2008 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

exception InvalidJID
 
type jid = {
  string: string;
  node: string;
  lnode: string;
  domain: string;
  ldomain: string;
  resource: string;
  lresource: string
}

let jid_of_string str =
  let bare_jid, resource = 
    try
	    let r = String.index str '/' in
	      String.sub str 0 r,
	    String.sub str (r+1) (String.length str - (r+1))
    with Not_found -> str, ""
  in
  let node, domain =
    if bare_jid = "" then
      raise InvalidJID
    else
      try
	      let s = String.index bare_jid '@' in
	        String.sub bare_jid 0 s,
	      String.sub bare_jid (s+1) (String.length bare_jid - (s+1))
      with Not_found ->
	      "", bare_jid
  in
    {
	    string = str;
	    node = node;
	    domain = domain;
	    resource = resource;
	    lnode = Stringprep.nodeprep node;
	    ldomain = Stringprep.nameprep domain; 
	    lresource = Stringprep.resourceprep resource
    }
      
let bare_jid jid = 
  { jid with resource = ""; lresource = "" }
    
let string_of_jid ?(lowercase=true) jid =
  let node, domain, resource =
    match lowercase with
      | true -> jid.lnode, jid.ldomain, jid.lresource
      | _ -> jid.node, jid.domain, jid.resource
  in
  let bare_jid = if node = "" then domain else node ^ "@" ^ domain in
    if resource = "" then bare_jid else bare_jid ^ "/" ^ resource
        
let tolower = string_of_jid ~lowercase:true
  
let is_bare jid = jid.lresource = ""
  
let is_node jid = jid.lnode <> ""
  
let is_bare_node jid = jid.lnode <> "" && jid.lresource = ""

let is_domain jid = jid.lnode = "" && jid.lresource = ""

let equal jid1 jid2 =
  jid1.lnode = jid2.lnode &&
  jid1.ldomain = jid2.ldomain &&
  jid1.lresource = jid2.lresource
  
