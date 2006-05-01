(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml
open Xmpp

type x_data_type = [
| `BOOLEAN
| `FIXED
| `HIDDEN
| `JID_MULTI
| `JID_SINGLE
| `LIST_MULTI
| `LIST_SINGLE
| `TEXT_MULTI
| `TEXT_PRIVATE
| `TEXT_SINGLE
]
let make_x_data_field ?label ?var ?value ?required ?options ?type_ () =
   let a1 = match type_ with
      | None -> []
      | Some t -> ["type", match t with
		      | `BOOLEAN -> "boolean"
		      | `FIXED -> "fixed"
		      | `HIDDEN -> "hidden"
		      | `JID_MULTI -> "jid-multi"
		      | `JID_SINGLE -> "jid-single"
		      | `LIST_MULTI -> "list-multi"
		      | `LIST_SINGLE -> "list-single"
		      | `TEXT_MULTI -> "text-multi"
		      | `TEXT_PRIVATE -> "text-private"
		      | `TEXT_SINGLE -> "text-single"] in
   let a2 = match label with
      | None -> a1
      | Some l -> ("label", l) :: a1
   in
   let a3 = match var with
      | None -> a2
      | Some v -> ("var", v) :: a2
   in
   let s1 =  match required with
      | Some true -> [Xmlelement ("required", [], [])]
      | Some false
      | None -> [] in
   let s2 = match value with
      | None -> s1
      | Some v -> make_simple_cdata "value" v :: s1 in
   let s3 = match options with
      | None -> s2
      | Some opts -> 
	   s2 @ List.map (function l, v ->
			     Xmlelement ("option", [("label", l)],
					 [make_simple_cdata "value" v])
                         ) opts in
      Xmlelement ("field", a3, s3)

type xdata_t = [
| `Form
| `Submit
| `Cancel
| `Result
]

let make_x_data ?title ?instructions fieldlist (type_:xdata_t) =
   let s1 =
      match instructions with
	 | None -> fieldlist
	 | Some instr ->
	      make_simple_cdata "instructions" instr:: fieldlist
   in
   let s2 =
      match title with
	 | None -> s1
	 | Some t -> make_simple_cdata "title" t :: s1
   in
      Xmlelement ("x", [("xmlns", "jabber:x:data"); ("type", match type_ with
							| `Form -> "form"
							| `Submit -> "submit"
							| `Cancel -> "cancel"
							| `Result -> "result")],
		  s2)

let get_x_data_type xdata =
   match safe_get_attr_s xdata "type" with
      | "result" -> `Result
      | "cancel" -> `Cancel
      | "form" -> `Form
      | "submit" -> `Submit
      | _ -> raise InvalidProtocol

let get_x_data_fields xdata =
   match xdata with
      | Xmlelement ("x", attrs, subels) ->
           let values = ref [] in
              List.iter
                 (function f ->
                     match f with
                        | Xmlelement ("field", a, s) ->
                             let var = Xml.get_attr_s f "var" in
                             let value = Xml.get_cdata f ~path:["value"] in
                                values := (var, value) :: !values
                           | _ -> ()
                    ) subels;
              !values
      | _ -> raise Not_found

let get_x_data_bool value =
   if value = "0" || value = "no" || value = "false" then false
   else if value = "1" || value = "yes" || value = "true" then true
   else raise Not_found

