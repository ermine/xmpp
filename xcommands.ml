(*                                                                          *)
(* (c) 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>                    *)
(*                                                                          *)

open Xml
open Xmpp
open Error

type status = [
| `Executing
| `Completed
| `Canceled
]

type action = [
| `Execute
| `Cancel
| `Prev
| `Next
| `Complete
]

type info = [
| `Info of string
| `Warn of string
| `Error of string
]

type cmd_error = [
| `ERR_MALFORMED_ACTION (* The responding JID does not understand 
			   the specified action. *)
| `ERR_BAD_ACTION       (* The responding JID cannot accept the
			   specified action. *)
| `ERR_BAD_LOCALE       (* The responding JID cannot accept the
			   specified language/locale. *)
| `ERR_BAD_PAYLOAD     (* The responding JID cannot accept the 
			  specified payload (e.g. the data form
			  did not provide one or more required
			  fields). *)
| `ERR_BAD_SESSION     (* The responding JID cannot accept the 
			  specified sessionid. *)
| `ERR_SESSION_EXPIRED (* The requesting JID specified a
			  sessionid that is no longer active
			  (either because it was completed,
			  xcanceled, or timed out). *)
| `ERR_FORBIDDEN       (* The requesting JID is not allowed to
			  execute the command. *)
| `ERR_ITEM_NOT_FOUND  (* The responding JID cannot find the  
			  requested command node. *)
| `ERR_FEATURE_NOT_IMPLEMENTED (* The responding JID does not support 
				  "http://jabber.org/protocol/commands" *)
]

let new_sessionid from to_ node =
   node ^ ":" ^ string_of_float (Unix.gettimeofday ())

let make_command_reply xml ?sessionid ?(lang:string option) 
      node ?status ?note subels =
   let a1 = ["xmlns", "http://jabber.org/protocol/commands";
		"node", node] in
   let a2 =match sessionid with
      | None -> a1
      | Some id -> ("sessionid", id) :: a1 in
   let a3 = match status with
      | None -> a2
      | Some s ->
	   ("status", (match s with
			 | `Executing -> "executing"
			 | `Completed -> "completed"
			 | `Canceled -> "canceled")) :: a2 in

   let subels = match note with
      | None -> subels
      | Some n ->
	   (match n with
	       | `Info text ->
		    Xmlelement ("note", ["type", "info"], [Xmlcdata text])
	       | `Wart text ->
		    Xmlelement ("note", ["type", "warn"], [Xmlcdata text])
	       | `Error text ->
		    Xmlelement ("note", ["type", "error"], [Xmlcdata text]))
	      :: subels in

      match xml with
	 | Xmlelement (_, attrs, _) ->
	      let newattrs = make_attrs_reply ?lang ~type_:"result" attrs in
		 Xmlelement ("iq", newattrs, 
			     [Xmlelement ("command", a3, subels)])

	 | _ -> raise NonXmlelement

let make_command_error xml ?text error =
   let xmpp_error, specific_cond =
      let attr = ["xmlns", "http://jabber.org/protocol/command"] in
	 match error with
	    | `ERR_MALFORMED_ACTION ->
		 `ERR_BAD_REQUEST, 
		 Some (Xmlelement ("cmd:malformed-action", attr, []))
	    | `ERR_BAD_ACTION ->
		 `ERR_BAD_REQUEST, 
		 Some (Xmlelement ("cmd:bad-action", attr, []))
	    | `ERR_BAD_PAYLOAD ->
		 `ERR_BAD_REQUEST, 
		 Some (Xmlelement ("cmd:bad-payload", attr, []))
	    | `ERR_BAD_SESSIONID ->
		 `ERR_BAD_REQUEST, 
		 Some (Xmlelement ("cmd:bad-sessionid", attr, []))
	    | `ERR_SESSION_EXPIRED ->
		 `ERR_NOT_ALLOWED, 
		 Some (Xmlelement ("cmd:session-expired", attr, []))
	    | `ERR_FORBIDDEN ->
		 `ERR_FORBIDDEN, None
	    | `ERR_ITEM_NOT_FOUND ->
		 `ERR_ITEM_NOT_FOUND, None
	    | `ERR_FEATURE_NOT_IMPLEMENTED ->
		 `ERR_FEATURE_NOT_IMPLEMENTED, None
		    
   in
      make_error_reply xml ?text ?specific_cond xmpp_error

let command_info xml =
   let command = get_by_xmlns xml ~tag:"command" 
      "http://jabber.org/protocol/commands" in
   let node = safe_get_attr_s command "node" in
   let sessionid = safe_get_attr_s command "sessionid" in
   let action = 
      let a = safe_get_attr_s command "action" in
	 match a with
	    | "execute" -> `Execute
	    | "prev" -> `Prev
	    | "next" -> `Next
	    | "cancel" -> `Cancel
	    | "complete" -> `Complete
	    | _ -> `Execute
   in
      node, action, sessionid
