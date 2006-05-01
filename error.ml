(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru>                   *)
(*                                                                          *)
open Xmpp
open Xml

exception UnknownError

type error = [
| `ERR_BAD_REQUEST
| `ERR_CONFLICT
| `ERR_FEATURE_NOT_IMPLEMENTED
| `ERR_FORBIDDEN
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

type error_type = [
| `Cancel
| `Continue
| `Modify
| `Auth
| `Wait
]

let error_to_tuple (err:error) =
   match err with
      | `ERR_BAD_REQUEST -> 
	   "400", "modify", "bad-request"
      | `ERR_CONFLICT -> 
	   "409", "cancel", "conflict"
      | `ERR_FEATURE_NOT_IMPLEMENTED -> 
	   "501", "cancel", "feature-not-implemented"
      | `ERR_FORBIDDEN -> 
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
      | "404" -> `ERR_ITEM_NOT_FOUND
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

let cond_to_error cond =
   match cond with
      | "bad-request" -> `ERR_BAD_REQUEST
      | "conflict" -> `ERR_CONFLICT
      | "feature-not-implemented" ->`ERR_FEATURE_NOT_IMPLEMENTED
      | "forbidden" -> `ERR_FORBIDDEN
      | "gone" -> `ERR_GONE
      | "internal-server-error" -> `ERR_INTERNAL_SERVER_ERROR
      | "item-not-found" -> `ERR_ITEM_NOT_FOUND
      | "jid-malformed" -> `ERR_JID_MALFORMED
      | "not-acceptable" -> `ERR_NOT_ACCEPTABLE
      | "not-allowed" -> `ERR_NOT_ALLOWED
      | "not-authorized" -> `ERR_NOT_AUTHORIZED
      | "payment-required" -> `ERR_PAYMENT_REQUIRED
      | "recipient-unavailable" -> `ERR_RECIPIENT_UNAVAILABLE
      | "redirect" -> `ERR_REDIRECT
      | "registration-required" -> `ERR_REGISTRATION_REQUIRED
      | "remote-server-not-found" -> `ERR_REMOTE_SERVER_NOT_FOUND
      | "remote-server-timeout" -> `ERR_REMOTE_SERVER_TIMEOUT
      | "resource-constraint" -> `ERR_RESOURCE_CONSTRAINT
      | "service-unavailable" -> `ERR_SERVICE_UNAVAILABLE
      | "subscription-required" -> `ERR_SUBSCRIPTION_REQUIRED
      | "undefuned-condition" -> `ERR_UNDEFINED_CONDITION
      | "unexpected-request" -> `ERR_UNEXPECTED_REQUEST
      | _ -> raise UnknownError

let parse_stream_error els =
   let cond = ref `ERR_UNDEFINED_CONDITION in
   let text = ref "" in
   let rec aux_iter tail acc =
      match tail with
	 | [] -> List.rev acc
	 | x :: xs ->
	      (match x with
		  | Xmlelement (name, attrs, els) ->
		       if List.assoc "xmlns" attrs = 
			  "urn:ietf:params:xml:ns:xmpp-streams" then begin
			     try 
				cond := cond_to_error name 
			     with UnknownError ->
				match name with
				   | "text" -> text := get_cdata x
				   | _ -> ()
			  end
		  | _ -> ());
	      aux_iter xs acc
   in
   let rest = aux_iter els [] in
      !cond, !text, rest

let parse_error stanza =
   let err =
      try Xml.get_by_xmlns stanza ~tag:"error" 
	 "xurn:ietf:params:xml:ns:xmpp-stanzas" with Not_found ->
	    get_tag stanza ["error"]
   in
   let type_ =
      match try Xml.get_attr_s err "type" with _ -> "" with
	 | "cancel" -> `Cancel
	 | "continue" -> `Continue
	 | "modify" -> `Modify
	 | "auth" -> `Auth
	 | "wait" -> `Wait
	 | _ -> `Cancel
   in
   let cond = ref `ERR_UNDEFINED_CONDITION in
   let text = ref "" in
   let rec aux_iter tail acc =
      match tail with
	 | [] -> List.rev acc
	 | x :: xs ->
	      (match x with
		  | Xmlelement (name, attrs, els) ->
		       if List.assoc "xmlns" attrs = 
			  "urn:ietf:params:xml:ns:xmpp-stanzas" then begin
			     try 
				cond := cond_to_error name 
			     with UnknownError ->
				match name with
				   | "text" -> text := get_cdata x
				   | _ -> ()
			  end
		  | _ -> ());
	      aux_iter xs acc
   in
   let rest = aux_iter (get_subels err) [] in
      begin
	 try
	    if !cond = `ERR_UNDEFINED_CONDITION then
	       let code = get_attr_s err "code" in
		  cond := code_to_error code
	 with _ -> ()
      end;
      !cond, type_, !text, rest

let make_error_reply (xml:Xml.element) ?(text:string option)
      ?(text_lang:string option) ?specific_cond (error:error) =
   let code, err_type, cond = error_to_tuple error in
   let el1 =
      Xmlelement (cond, ["xmlns", "urn:ietf:params:xml:ns:xmpp-stanzas"], []) in
   let el2 =
      match text with
	 | Some text ->
	      [el1;
	       Xmlelement ("text", 
			   ["xmlns", "urn:ietf:params:xml:ns:xmpp-stanzas";
			    "xml:lang", (match text_lang with
					    | None -> "en"
					    | Some lang -> lang)],
			   [Xmlcdata text])]
	 | None -> [el1] in
   let el3 =
      match specific_cond with
	 | None -> el2
	 | Some el ->
	      el :: el2 in

      match xml with
	 | Xmlelement (name, attrs, subtags) ->
	      let a = make_attrs_reply attrs ~type_:"error" in
		 Xmlelement (name, a,
                             subtags @ 
				[Xmlelement
                                    ("error",["code", code;
					      "type", err_type], el3)])
	 | _ -> raise NonXmlelement
