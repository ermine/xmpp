(*
 * (c) 2004, 2005, 2006, 2007 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

type jid = {
  string : string;
  node : string;
  lnode : string;
  domain : string;
  ldomain : string;
  resource : string;
  lresource : string;
}

val jid_of_string : string -> jid
val bare_jid: jid -> jid
val string_of_jid : ?lowercase:bool -> jid -> string
val tolower : jid -> string

val is_bare : jid -> bool
val is_node : jid -> bool
val is_bare_node : jid -> bool
val is_domain : jid -> bool
val equal : jid -> jid -> bool
