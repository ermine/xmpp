(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *)

open Xml

let ns_xmpp_streams = Some "urn:ietf:params:xml:ns:xmpp-streams"
let ns_streams = Some "http://etherx.jabber.org/streams"

type condition =
  | ERR_BAD_FORMAT
  | ERR_BAD_NAMESPACE_PREFIX
  | ERR_CONFLICT
  | ERR_CONNECTION_TIMEOUT
  | ERR_HOST_GONE
  | ERR_HOST_UNKNOWN
  | ERR_IMPROPER_ADDRESSING
  | ERR_INTERNAL_SERVER_ERROR
  | ERR_INVALID_FROM
  | ERR_INVALID_ID
  | ERR_INVALID_NAMESPACE
  | ERR_INVALID_XML
  | ERR_NOT_AUTHORIZED
  | ERR_POLICY_VIOLATION
  | ERR_REMOTE_CONNECTION_FAILED
  | ERR_RESOURCE_CONSTRAINT
  | ERR_RESTRICTED_XML
  | ERR_SEE_OTHER_HOST
  | ERR_SYSTEM_SHUTDOWN
  | ERR_UNDEFINED_CONDITION
  | ERR_UNSUPPORTED_ENCODING
  | ERR_UNSUPPORTED_STANZA_TYPE
  | ERR_UNSUPPORTED_VERSION
  | ERR_XML_NOT_WELL_FORMED
  | UNKNOWN_CONDITION of string
      
let string_of_condition = function
  | ERR_BAD_FORMAT -> "bad-format"
  | ERR_BAD_NAMESPACE_PREFIX -> "bad-namespace-prefix"
  | ERR_CONFLICT -> "conflict"
  | ERR_CONNECTION_TIMEOUT -> "connection-timeout"
  | ERR_HOST_GONE -> "host-gone"
  | ERR_HOST_UNKNOWN -> "host-unknown"
  | ERR_IMPROPER_ADDRESSING -> "improper-addressing"
  | ERR_INTERNAL_SERVER_ERROR -> "internal-server-error"
  | ERR_INVALID_FROM -> "invalid-form"
  | ERR_INVALID_ID -> "invalid-id"
  | ERR_INVALID_NAMESPACE -> "invalid-namespace"
  | ERR_INVALID_XML -> "invalid-xml"
  | ERR_NOT_AUTHORIZED -> "not-authorized"
  | ERR_POLICY_VIOLATION -> "policy-violation"
  | ERR_REMOTE_CONNECTION_FAILED -> "remote-connection-failed"
  | ERR_RESOURCE_CONSTRAINT -> "resource-constraint"
  | ERR_RESTRICTED_XML -> "restricted-xml"
  | ERR_SEE_OTHER_HOST -> "see-other-host"
  | ERR_SYSTEM_SHUTDOWN -> "system-shutdown"
  | ERR_UNDEFINED_CONDITION -> "undefined-condition"
  | ERR_UNSUPPORTED_ENCODING -> "unsupported-encoding"
  | ERR_UNSUPPORTED_STANZA_TYPE -> "unsupported-stanza-type"
  | ERR_UNSUPPORTED_VERSION -> "unsupported-version"
  | ERR_XML_NOT_WELL_FORMED -> "xml-not-well-formed"
  | UNKNOWN_CONDITION other -> other

let condition_of_string = function
  | "bad-format" -> ERR_BAD_FORMAT
  | "bad-namespace-prefix" -> ERR_BAD_NAMESPACE_PREFIX
  | "conflict" -> ERR_CONFLICT
  | "connection-timeout" -> ERR_CONNECTION_TIMEOUT
  | "host-gone" -> ERR_HOST_GONE
  | "host-unknown" -> ERR_HOST_UNKNOWN
  | "improper-addressing" -> ERR_IMPROPER_ADDRESSING
  | "internal-server-error" -> ERR_INTERNAL_SERVER_ERROR
  | "invalid-form" -> ERR_INVALID_FROM
  | "invalid-id" -> ERR_INVALID_ID
  | "invalid-namespace" -> ERR_INVALID_NAMESPACE
  | "invalid-xml" -> ERR_INVALID_XML
  | "not-authorized" -> ERR_NOT_AUTHORIZED
  | "policy-violation" -> ERR_POLICY_VIOLATION
  | "remote-connection-failed" -> ERR_REMOTE_CONNECTION_FAILED
  | "resource-constraint" -> ERR_RESOURCE_CONSTRAINT
  | "restricted-xml" -> ERR_RESTRICTED_XML
  | "see-other-host" -> ERR_SEE_OTHER_HOST
  | "system-shutdown" -> ERR_SYSTEM_SHUTDOWN
  | "undefined-condition" -> ERR_UNDEFINED_CONDITION
  | "unsupported-encoding" -> ERR_UNSUPPORTED_ENCODING
  | "unsupported-stanza-type" -> ERR_UNSUPPORTED_STANZA_TYPE
  | "unsupported-version" -> ERR_UNSUPPORTED_VERSION
  | "xml-not-well-formed" -> ERR_XML_NOT_WELL_FORMED
  | other -> UNKNOWN_CONDITION other

type t = {
  err_condition: condition;
  err_text: string;
}

let parse_error els =
  let (cond, text) =
    List.fold_left (fun (cond, text) -> function
                      | Xmlelement ((ns, name), _, _) as el ->
                          if (ns, name) = (ns_xmpp_streams, "text") then
                            let text = get_cdata el in
                              (cond, text)
                          else if ns = ns_xmpp_streams then
                            let cond = condition_of_string name in
                              (cond, text)
                          else
                            (cond, text)
                      | Xmlcdata _ ->
                          (cond, text)
                   ) (UNKNOWN_CONDITION "", "") els in
    {err_condition = cond; err_text = text}
      
let make_error ?text ?lang ?app_condition condition =
  let text_el =
    match text with
      | None -> []
      | Some text ->
          [make_element (ns_xmpp_streams, "text")
             (match lang with
                | None -> []
                | Some v -> [make_attr ~ns:ns_xml "lang" v])
             [Xmlcdata text]]
  in
    make_element (ns_streams, "error") []
      ((make_element (ns_xmpp_streams, string_of_condition condition) [] []) ::
         (match app_condition with
            | None -> text_el
            | Some el -> el :: text_el)
      )
    
