(*
 * (c) 2004-2013 Anastasia Gornostaeva
 *)

exception MalformedJID

type t = {
  node : string;
  lnode : string;
  domain : string;
  ldomain : string;
  resource : string;
  lresource : string;
}

val of_string : ?strong:bool -> string -> t
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
val make_jid : ?strong:bool -> string -> string -> string -> t
val replace_resource : ?strong:bool -> t -> string -> t

val nodeprep : ?strong:bool -> string -> string
val nameprep : ?strong:bool -> string -> string
val resourceprep : ?strong:bool -> string -> string
