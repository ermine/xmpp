(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru>                   *)
(*                                                                          *)
open Xmpp
open Xml

type error = [
| `ERR_BAD_REQUEST
| `ERR_CONFLICT
| `ERR_FEATURE_NOT_IMPLEMENTED
| `ERR_FORRBIDEN
| `ERR_GONE
| `ERR_INTERNAL_SERVER_ERROR
| `ERR_ITEM_NOT_FOUND
| `ERR_JID_MALFORMED
| `ERR_NOT_ACCEPTABLE
| `ERR_NOT_ALLOWED
| `ERR_NOT_AUTHORIZED
| `ERR_PAYMENT_REQUIRED
| `ERR_RECIPIENT_UNAVAILABLE
| `ERR_REDIRECT
| `ERR_REGISTRATION_REQUIRED
| `ERR_REMOTE_SERVER_NOT_FOUND
| `ERR_REMOTE_SERVER_TIMEOUT
| `ERR_RESOURCE_CONSTRAINT
| `ERR_SERVICE_UNAVAILABLE
| `ERR_SUBSCRIPTION_REQUIRED
| `ERR_UNDEFINED_CONDITION
| `ERR_UNEXPECTED_REQUEST
]

let error_to_tuple (err:error) =
   match err with
      | `ERR_BAD_REQUEST -> 
	   "400", "modufy", "bad-request"
      | `ERR_CONFLICT -> 
	   "409", "cancel", "conflict"
      | `ERR_FEATURE_NOT_IMPLEMENTED -> 
	   "501", "cancel", "feature-not-implemented"
      | `ERR_FORRBIDEN -> 
	   "403", "auth", "forbidden"
      | `ERR_GONE -> 
	   "302", "modify", "gone"
      | `ERR_INTERNAL_SERVER_ERROR -> 
	   "500", "wait", "internal-server-error"
      | `ERR_ITEM_NOT_FOUND -> 
	   "404", "cancel", "item-not-found"
      | `ERR_JID_MALFORMED -> 
	   "400", "modify", "jid-malformed"
      | `ERR_NOT_ACCEPTABLE -> 
	   "406", "modify", "not-acceptable"
      | `ERR_NOT_ALLOWED -> 
	   "405", "cancel", "not-allowed"
      | `ERR_NOT_AUTHORIZED -> 
	   "401", "auth", "not-authorized"
      | `ERR_PAYMENT_REQUIRED -> 
	   "402", "auth", "payment-required"
      | `ERR_RECIPIENT_UNAVAILABLE -> 
	   "404", "wait", "recipient-unavailable"
      | `ERR_REDIRECT -> 
	   "302", "modify", "redirect"
      | `ERR_REGISTRATION_REQUIRED -> 
	   "407", "auth", "registration-required"
      | `ERR_REMOTE_SERVER_NOT_FOUND -> 
	   "4004", "cancel", "remote-server-not-found"
      | `ERR_REMOTE_SERVER_TIMEOUT -> 
	   "404", "wait", "remote-server-timeout"
      | `ERR_RESOURCE_CONSTRAINT -> 
	   "500", "wait", "resource-constraint"
      | `ERR_SERVICE_UNAVAILABLE -> 
	   "503", "wait", "service-unavailable"
      | `ERR_SUBSCRIPTION_REQUIRED -> 
	   "407", "auth", "subscription-required"
      | `ERR_UNEXPECTED_REQUEST ->
	   "500", "wait", "unexpected-request"
      | `ERR_UNDEFINED_CONDITION ->
	   "400", "cancel", "undefined-condition"

let code_to_error code =
   match code with
      | "302" -> `ERR_REDIRECT
      | "400" -> `ERR_BAD_REQUEST
      | "401" -> `ERR_NOT_AUTHORIZED
      | "402" -> `ERR_PAYMENT_REQUIRED
      | "403" -> `ERR_FORBIDDEN
      | "404" -> `ERR_NOT_FOUND
      | "405" -> `ERR_NOT_ALLOWED
      | "406" -> `ERR_NOT_ACCEPTABLE
      | "407" -> `ERR_REGISTRATION_REQUIRED
      | "408" -> `ERR_REMOTE_SERVER_TIMEOUT
      | "409" -> `ERR_CONFLICT
      | "500" -> `ERR_INTERNAL_SERVER_ERROR
      | "501" -> `ERR_FEATURE_NOT_IMPLEMENTED
      | "502" -> `ERR_SERVICE_UNAVAILABLE
      | "503" -> `ERR_SERVICE_UNAVAILABLE
      | "504" -> `ERR_REMOTE_SERVER_TIMEOUT
      | "510" -> `ERR_SERVICE_UNAVAILABLE
      | _ -> `ERR_UNDEFINED_CONDITION

let make_error_reply (xml:Xml.element) ?(descr:string option) (error:error) =
   let code, err_type, cond = error_to_tuple error in
   let el1 =
      Xmlelement (cond, ["value", "urn:ietf:params:xml:ns:xmpp-stanzas"], []) in
   let el2 =
      match descr with
	 | Some text ->
	      [el1;
	       Xmlelement ("text", 
			   ["xmlns", "urn:ietf:params:xml:ns:xmpp-stanzas";
			    "xml:lang", "en"],
			   [Xmlcdata text])]
	 | None -> [el1]
   in
      match xml with
	 | Xmlelement (name, attrs, subtags) ->
	      let a = make_attrs_reply attrs ~type_:"error" in
		 Xmlelement (name, a,
                             subtags @ 
				[Xmlelement
                                    ("error",
                                     [("code", code)], el2)])
	 | _ -> raise NonXmlelement
