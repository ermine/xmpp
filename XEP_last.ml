(*
 * (c) 2004-2010 Anastasia Gornostaeva
 *
 * XEP-0012: Last Activity
 * Version: 2.0
 *)

open Xml

let ns_last = Some "jabber:iq:last"

type t = {
  seconds : int;    (* required *)
  reason : string   (* optional *)
}
  
let encode ?(reason="") seconds =
  make_element (ns_last, "query")
    [make_attr "seconds" (string_of_int seconds)]
    [Xmlcdata reason]

let decode el =
  let attrs = get_attrs el in
    try
      let seconds = int_of_string (get_attr_value "seconds" attrs) in
      let reason = get_cdata el in
        Some {seconds = seconds; reason = reason}
    with _ -> None

let make_iq_get () =
  make_element (ns_last, "query") [] []
