(*
 * (c) 2004-2012 Anastasia Gornostaeva
 *)

module Make (X : XMPP.S) =
struct
  let ns_xdata = Some "jabber:x:data"

  let encode () =
    ()
    
  let decode el =
    ()
end
