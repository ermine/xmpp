(*
 * (c) 2004-2009 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

open Xml
  
exception Error of string
  
let ns_xmpp_stanzas = Some "urn:ietf:params:xml:ns:xmpp-stanzas"

type error_type =
  | Cancel
  | Continue
  | Modify
  | Auth
  | Wait

let string_of_error_type = function
  | Cancel -> "cancel"
  | Continue -> "continue"
  | Modify -> "modify"
  | Auth -> "auth"
  | Wait -> "wait"

type condition =
  | ERR_BAD_REQUEST
  | ERR_CONFLICT
  | ERR_FEATURE_NOT_IMPLEMENTED
  | ERR_FORBIDDEN
  | ERR_GONE
  | ERR_INTERNAL_SERVER_ERROR
  | ERR_ITEM_NOT_FOUND
  | ERR_JID_MALFORMED
  | ERR_NOT_ACCEPTABLE
  | ERR_NOT_ALLOWED
  | ERR_NOT_AUTHORIZED
  | ERR_PAYMENT_REQUIRED
  | ERR_RECIPIENT_UNAVAILABLE
  | ERR_REDIRECT
  | ERR_REGISTRATION_REQUIRED
  | ERR_REMOTE_SERVER_NOT_FOUND
  | ERR_REMOTE_SERVER_TIMEOUT
  | ERR_RESOURCE_CONSTRAINT
  | ERR_SERVICE_UNAVAILABLE
  | ERR_SUBSCRIPTION_REQUIRED
  | ERR_UNDEFINED_CONDITION
  | ERR_UNEXPECTED_REQUEST
  | UNKNOWN_CONDITION of string

let condition_of_string = function
  | "bad-request" -> ERR_BAD_REQUEST
  | "conflict" -> ERR_CONFLICT
  | "feature-not-implemented" -> ERR_FEATURE_NOT_IMPLEMENTED
  | "forbidden" -> ERR_FORBIDDEN
  | "gone" -> ERR_GONE
  | "internal-server-error" -> ERR_INTERNAL_SERVER_ERROR
  | "item-not-found" -> ERR_ITEM_NOT_FOUND
  | "jid-malformed" -> ERR_JID_MALFORMED
  | "not-acceptable" -> ERR_NOT_ACCEPTABLE
  | "not-allowed" -> ERR_NOT_ALLOWED
  | "not-authorized" -> ERR_NOT_AUTHORIZED
  | "payment-required" -> ERR_PAYMENT_REQUIRED
  | "recipient-unavailable" -> ERR_RECIPIENT_UNAVAILABLE
  | "redirect" -> ERR_REDIRECT
  | "registration-required" -> ERR_REGISTRATION_REQUIRED
  | "remote-server-not-found" -> ERR_REMOTE_SERVER_NOT_FOUND
  | "remote-server-timeout" -> ERR_REMOTE_SERVER_TIMEOUT
  | "resource-constraint" -> ERR_RESOURCE_CONSTRAINT
  | "service-unavailable" -> ERR_SERVICE_UNAVAILABLE
  | "subscription-required" -> ERR_SUBSCRIPTION_REQUIRED
  | "undefined-condition" -> ERR_UNDEFINED_CONDITION
  | "unexpected-request" -> ERR_UNEXPECTED_REQUEST
  | other -> UNKNOWN_CONDITION other

let string_of_condition = function
  | ERR_BAD_REQUEST -> "bad-request"
  | ERR_CONFLICT -> "conflict"
  | ERR_FEATURE_NOT_IMPLEMENTED -> "feature-not-implemented"
  | ERR_FORBIDDEN -> "forbidden"
  | ERR_GONE -> "gone"
  | ERR_INTERNAL_SERVER_ERROR -> "internal-server-error"
  | ERR_ITEM_NOT_FOUND -> "item-not-found"
  | ERR_JID_MALFORMED -> "jid-malformed"
  | ERR_NOT_ACCEPTABLE -> "not-acceptable"
  | ERR_NOT_ALLOWED -> "not-allowed"
  | ERR_NOT_AUTHORIZED -> "not-authorized"
  | ERR_PAYMENT_REQUIRED -> "payment-required"
  | ERR_RECIPIENT_UNAVAILABLE -> "recipient-unavailable"
  | ERR_REDIRECT -> "redirect"
  | ERR_REGISTRATION_REQUIRED -> "registration-required"
  | ERR_REMOTE_SERVER_NOT_FOUND -> "remote-server-not-found"
  | ERR_REMOTE_SERVER_TIMEOUT -> "remote-server-timeout"
  | ERR_RESOURCE_CONSTRAINT -> "resource-constraint"
  | ERR_SERVICE_UNAVAILABLE -> "service-unavailable"
  | ERR_SUBSCRIPTION_REQUIRED -> "subscription-required"
  | ERR_UNDEFINED_CONDITION -> "undefined-condition"
  | ERR_UNEXPECTED_REQUEST -> "unexpected-request"
  | UNKNOWN_CONDITION _ -> raise (Error "unknown condition")

let error_type_of_condition = function
  | ERR_BAD_REQUEST -> Modify
  | ERR_CONFLICT -> Cancel
  | ERR_FEATURE_NOT_IMPLEMENTED -> Cancel
  | ERR_FORBIDDEN -> Cancel
  | ERR_GONE -> Modify
  | ERR_INTERNAL_SERVER_ERROR -> Wait
  | ERR_ITEM_NOT_FOUND -> Cancel
  | ERR_JID_MALFORMED -> Modify
  | ERR_NOT_ACCEPTABLE -> Modify
  | ERR_NOT_ALLOWED -> Cancel
  | ERR_NOT_AUTHORIZED -> Auth
  | ERR_PAYMENT_REQUIRED -> Auth
  | ERR_RECIPIENT_UNAVAILABLE -> Wait
  | ERR_REDIRECT -> Modify
  | ERR_REGISTRATION_REQUIRED -> Auth
  | ERR_REMOTE_SERVER_NOT_FOUND -> Cancel
  | ERR_REMOTE_SERVER_TIMEOUT -> Wait
  | ERR_RESOURCE_CONSTRAINT -> Wait
  | ERR_SERVICE_UNAVAILABLE -> Cancel
  | ERR_SUBSCRIPTION_REQUIRED -> Auth
  | ERR_UNDEFINED_CONDITION -> Cancel
  | ERR_UNEXPECTED_REQUEST -> Wait
  | UNKNOWN_CONDITION _ -> raise (Error "unknown condition")
      
type t = {
  err_type: error_type;
  err_condition: condition;
  err_text: string;
  err_lang: string
}

let parse_error error =
  let err_type =
    match safe_get_attr_value "type" (get_attrs error) with
      | "cancel" -> Cancel
      | "continue" -> Continue
      | "modify" -> Modify
      | "auth" -> Auth
      | "wait" -> Wait
      | _ -> Cancel
  in
  let t = List.fold_left (fun r -> function
                            | Xmlelement ((ns, name), attrs, _) as el ->
                                if (ns_xmpp_stanzas, "text")  = (ns, name) then
                                  let lang =
                                    safe_get_attr_value ~ns:ns_xml "lang" attrs
                                  in
                                  let text = get_cdata el in
                                    {r with err_text = text; err_lang = lang}
                                else if ns = ns_xmpp_stanzas then
                                  let cond = condition_of_string name in
                                    {r with err_condition = cond}
                                else
                                  r
                            | Xmlcdata _ ->
                                r
                         ) { err_type = err_type;
                             err_condition = UNKNOWN_CONDITION "";
                             err_text = "";
                             err_lang = ""
                           } (get_children error) in
    t
      
let create_error ?type_ ?text ?lang condition =
  {err_type = (match type_ with
                 | None -> error_type_of_condition condition
                 | Some v -> v);
   err_condition = condition;
   err_text = (match text with None -> "" | Some v -> v);
   err_lang = (match lang with None -> "" | Some v -> v);
  }

let of_error ns err =
  make_element (ns, "error")
    [make_attr "type" (string_of_error_type err.err_type)]
    (make_element (ns_xmpp_stanzas, string_of_condition err.err_condition) [] []
     ::
     (if err.err_text = "" then
        []
      else
        [make_element (ns_xmpp_stanzas, "text")
           [make_attr ~ns:ns_xml "lang"
              (if err.err_lang = "" then "en" else err.err_lang)] []])
    )

let make_error ~ns ?error_type ?(text="") ?(lang="") condition =
  let err =
    {err_type = (match error_type with
                   | None -> error_type_of_condition condition;
                   | Some v -> v
                );
     err_condition = condition;
     err_text = text;
     err_lang = lang
    } in
    of_error ns err
