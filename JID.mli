(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

type t = {
  node : string;
  lnode : string;
  domain : string;
  ldomain : string;
  resource : string;
  lresource : string;
}

val of_string : string -> t
val bare_jid : t -> t
val domain : t -> t
val string_of_jid : ?lowercase:bool -> t -> string
val tolower : t -> string

val is_bare : t -> bool
val is_node : t -> bool
val is_bare_node : t -> bool
val is_domain : t -> bool
val equal : t -> t -> bool
val compare : t -> t -> int
val make_jid : string -> string -> string -> t
val replace_resource : t -> string -> t
