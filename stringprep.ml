(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)
(* based on ejabberd/stringprep (c) Alexey Shchepin                         *)

open Uni_data
open Uni_norm

exception StringprepError of string

let get_decomp uc =
   let info = getUniCharDecompInfo uc in
      if info >= 0 then
	 let decomp_len = getDecompLen info in
	 let decomp_shift = getDecompShift info in
	 let rec aux j =
	    if j < decomp_len then
	       decompList.(decomp_shift + j) :: aux (j+1)
	    else
	       []
	 in
	    aux 0
      else
	 [uc]

let canonical_ordering uar =
   let rec aux i last =
      if i < Array.length uar - 1 then
	 let next = getUniCharCClass uar.(i+1) in
	    if next <> 0 && last > next then
	       let rec aux2 j =
		  if j >= 0 then
		     if getUniCharCClass uar  .(j) <= next then ()
		     else
			let tmp = uar  .(j+1) in
			   uar  .(j+1) <- uar  .(j);
			   uar  .(j) <- tmp;
			   aux2 (j-1)
	       in
		  aux2 i;
		  aux (i+1) last
	    else
	       aux (i+1) next
      else
	 uar
   in
      aux 0 (getUniCharCClass uar.(0))

(* constants for hangul syllable [de]composition *)
let sBase = 0xAC00
let lBase = 0x1100
let vBase = 0x1161
let tBase = 0x11A7
let lCount = 19
let vCount = 21
let tCount = 28
let nCount = (vCount * tCount)
let sCount = (lCount * nCount)

(*
let decompose_hangul uchar =
   let sIndex = uchar - sBase in
      (* not a hangul syllable *)
      if sIndex < 0 || sIndex >= sCount then
	 [uchar]
      else
	 let l = lBase + sIndex / nCount in
	 let v = vBase + (sIndex mod nCount) / tCount in
	 let t = tBase + sIndex mod tCount in
	    [l; v]
*)

let utf8_width u =
   if u <= 0x7f then 1
   else if u <= 0x7ff then 2
   else if u <= 0xffff then 3
   else if u <= 0x10ffff then 4
   else raise (StringprepError 
		  ("cannot define width of code " ^ string_of_int u))

let compose ch1 ch2 =
   if lBase <= ch1 && ch1 < lBase + lCount &&
      vBase <= ch2 && ch2 < vBase + vCount then
	 sBase + ((ch1 - lBase) * vCount + (ch2 - vBase)) * tCount
   else if sBase <= ch1 && ch1 < sBase + sCount && 
      ((ch1 - sBase) mod tCount) = 0
	 && tBase <= ch2 && ch2 < tBase + tCount then
	    ch1 + ch2 - tBase
   else
      let info1 = getUniCharCompInfo ch1 in
      let info2 = getUniCharCompInfo ch2 in
	 if info1 <> -1 && info1 land compSingleMask > 0 then
	    if info1 land compSecondMask = 0  &&
               ch2 = compFirstList.(info1 land compMask).(0) then
		  compFirstList.(info1 land compMask).(1)
	    else
	       0
	 else if info2 <> -1 && info2 land compSingleMask > 0 then
	    if info2 land compSecondMask > 0 &&
	       ch1 = compSecondList.(info2 land compMask).(0) then
	       compSecondList.(info2 land compMask).(1)
	    else
	       0
	 else if info1 <> -1 && info2 <> -1 &&
	    info1 land compSecondMask = 0 && 
	    info2 land compSecondMask > 0 then
	    compBothList.(info1).(info2 land compMask)
	 else
	    0

let composite uar =
   let rec aux i ch1 prev_cc starterPos comp_pos =
      if i < Array.length uar then
	 let ch2 = uar.(i) in
	 let cc = getUniCharCClass ch2 in
	    if prev_cc = 0 || cc > prev_cc then
	       let composed = compose ch1 ch2 in
		  if composed > 0 then
		     aux (i+1) composed prev_cc starterPos comp_pos
		  else
		     if cc = 0 then begin
			uar.(starterPos) <- ch1;
			aux (i+1) ch2 0 comp_pos (comp_pos+1)
		     end
		     else begin
 			uar.(comp_pos) <- ch2;
			aux (i+1) ch1 cc starterPos (comp_pos+1)
		     end;
	    else
	       if cc = 0 then begin
		  uar.(starterPos) <- ch1;
		  aux (i+1) ch2 0 comp_pos (comp_pos+1)
	       end
	       else begin
 		  uar.(comp_pos) <- ch2;
		  aux (i+1) ch1 cc starterPos (comp_pos+1)
	       end
      else begin
	 uar.(starterPos) <- ch1;
	 Array.sub uar 0 comp_pos
      end
   in
      aux 1 uar.(0) (getUniCharCClass uar.(0)) 0 1

let print_utf8 str i =
   print_string "cannot convert from string to unicode: ";
   String.iter (fun s -> Printf.printf  "%X " (Char.code s))
      (String.sub str 0 i);
   print_newline ()

(* based on Ulex.Utf8.next *)
let next s i =
   match s.[i] with
      | '\000'..'\127' as c ->
           Char.code c, 1
      | '\192'..'\223' as c ->
           let n1 = Char.code c in
           let n2 = Char.code s.[i+1] in
(*
              if (n2 lsr 6 != 0b10) then begin
		 print_utf8 s 2;
		 raise (StringprepError "invalid unicode")
	      end;
*)
              ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f), 2
      | '\224' ->
           let n2 = Char.code s.[i+1] in
           let n3 = Char.code s.[i+2] in
(*
              if (n2 lsr 5 != 0b101) || (n3 lsr 6 != 0b10) then begin
		 print_utf8 s 3;
		 raise (StringprepError "invalid unicode")
	      end;
*)
              ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f), 3
      | ('\225'..'\236' | '\238'..'\239') as c ->
           let n1 = Char.code c in
           let n2 = Char.code s.[i+1] in
           let n3 = Char.code s.[i+2] in
(*
              if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then begin
		 print_utf8 s 3;
		 raise (StringprepError "invalid unicode")
	      end;
*)
              ((n1 land 0x0f) lsl 12) lor ((n2 land 0x3f) 
					   lsl 6) lor (n3 land 0x3f), 3
      | '\237' ->
           let n2 = Char.code s.[i+1] in
           let n3 = Char.code s.[i+2] in
(*
              if (n2 lsr 5 != 0b100) || (n3 lsr 6 != 0b10) then begin
		 print_utf8 s 3;
		 raise (StringprepError "invalid unicode")
	      end;
*)
              0xd000 lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f), 3
      | '\240' ->
           let n2 = Char.code s.[i+1] in
           let n3 = Char.code s.[i+2] in
           let n4 = Char.code s.[i+3] in
(*
              if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) || 
		 (n4 lsr 6 != 0b10) then begin
		    print_utf8 s 4;
		    raise (StringprepError "invalid unicode")
		 end;
*)
	      ((n2 land 0x3f) lsl 12) lor ((n3 land 0x3f) lsl 6) 
	      lor (n4 land 0x3f), 4
      | '\241'..'\243' as c ->
           let n1 = Char.code c in
           let n2 = Char.code s.[i+1] in
           let n3 = Char.code s.[i+2] in
           let n4 = Char.code s.[i+3] in
(*
              if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10)
              then begin
		 print_utf8 s 4;
		 raise (StringprepError "invalid unicode")
	      end;
*)
              ((n1 land 0x07) lsl 18) lor ((n2 land 0x3f) 
					   lsl 12) lor
		 ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f), 4
      | '\244' ->
           let n2 = Char.code s.[i+1] in
           let n3 = Char.code s.[i+2] in
           let n4 = Char.code s.[i+3] in
(*
              if (n2 lsr 4 != 0b1000) || (n3 lsr 6 != 0b10) || 
		 (n4 lsr 6 != 0b10) then begin
		    print_utf8 s 4;
		    raise (StringprepError "invalid unicode");
		 end;
*)
              0x100000 lor ((n2 land 0x3f) lsl 12) lor 
		 ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f), 4
      | _ -> 
	   print_utf8 s 4;
	   raise (StringprepError "invalid unicode")

(* based on Ulex.Utf.store *)
let store b p =
  if p <= 0x7f then
    Buffer.add_char b (Char.chr p)
  else if p <= 0x7ff then (
    Buffer.add_char b (Char.chr (0xc0 lor (p lsr 6)));
    Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
  )
  else if p <= 0xffff then (
(*    if (p >= 0xd800 & p < 0xe000) then raise MalFormed; *)
    Buffer.add_char b (Char.chr (0xe0 lor (p lsr 12)));
    Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6) land 0x3f)));
    Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
  )
  else if p <= 0x10ffff then (
    Buffer.add_char b (Char.chr (0xf0 lor (p lsr 18)));
    Buffer.add_char b (Char.chr (0x80 lor ((p lsr 12) land 0x3f)));
    Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6)  land 0x3f)));
    Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
  )
  else raise (StringprepError "cannot convert from unicode to utf8")
   
type t =
   | Nameprep
   | Nodeprep
   | Resourceprep

let stringprep ?(mode=Nameprep) str =
   let len = String.length str in
   let rec aux i =
      if i < len then
	 let uc, j = next str i in
	 let info = getUniCharInfo(uc) in
	    if info land b1Mask = 0 then
	       let  ruc =
		  if mode = Nodeprep || mode = Nameprep then
		     if info land mCMask = 0 then
			get_decomp (uc + getDelta info)
		     else
			let mc = getMC info in
			   List.flatten (List.map (fun m ->
						      get_decomp m) mc)
		  else
		     get_decomp uc;
	       in
		  ruc @ (aux (i+j))
	    else
	       aux (i+j)
      else
	 []
   in
   let decomposed = aux 0 in
      if decomposed  = [] then
	 ""
      else
	 let composed = composite (canonical_ordering 
				      (Array.of_list decomposed)) in

	 let prohibit =
	    match mode with
	       | Nameprep ->
		    aCMask
	       | Nodeprep ->
		    aCMask lor c11Mask lor c21Mask lor xNPMask
	 | Resourceprep ->
	      aCMask lor c21Mask
   in

(*
   let first_ral = getUniCharInfo composed.(0) land d1Mask in
*)
   let have_ral = ref false in
   let have_l = ref 0 in
   let last_ral = ref 0 in
   let buffer = Buffer.create (Array.length composed * 2) in

      Array.iter (fun ruc ->
		     let info = getUniCharInfo ruc in
			if info land prohibit > 0 then begin
(*
			   Printf.printf "Prohibit %d %X\n" 
			      ruc ruc;
*)
			   raise (StringprepError "prohibited");
			end;
			last_ral := info land d1Mask;
			have_ral := !have_ral || !last_ral > 0;
			have_l := info land d2Mask;
			store buffer ruc
		 ) composed;
(*
      if !have_ral && (first_ral > 0 || !last_ral > 0 || !have_l > 0) then
	 raise (StringprepError "invalid bidi");
*)
      Buffer.contents buffer

let lowercase str =
   let len = String.length str in
   let rec aux i =
      if i < len then
	 let uc, j = next str i in
	 let info = getUniCharInfo(uc) in
	    if info land b1Mask = 0 then
	       let  ruc =
		  if info land mCMask = 0 then
		     get_decomp (uc + getDelta info)
		  else
		     let mc = getMC info in
			List.flatten (List.map (fun m ->
						   get_decomp m) mc)
	       in
		  ruc @ (aux (i+j))
	    else
	       aux (i+j)
      else
	 []
   in
   let decomposed = aux 0 in
      if decomposed  = [] then
	 ""
      else
	 let composed = composite (canonical_ordering 
				      (Array.of_list decomposed)) in

(*
   let first_ral = getUniCharInfo composed.(0) land d1Mask in
*)
   let have_ral = ref false in
   let have_l = ref 0 in
   let last_ral = ref 0 in
   let buffer = Buffer.create (Array.length composed * 2) in

      Array.iter (fun ruc ->
		     let info = getUniCharInfo ruc in
			last_ral := info land d1Mask;
			have_ral := !have_ral || !last_ral > 0;
			have_l := info land d2Mask;
			store buffer ruc
		 ) composed;
(*
      if !have_ral && (first_ral > 0 || !last_ral > 0 || !have_l > 0) then
	 raise (StringprepError "invalid bidi");
*)
      Buffer.contents buffer
(***)  

let nameprep str = 
   if str <> "" then 
      stringprep ~mode:Nameprep str 
   else 
      ""

let nodeprep str = 
   if str <> "" then 
      stringprep ~mode:Nodeprep str 
   else 
      ""

let resourceprep str = 
   if str <> "" then 
      stringprep ~mode:Resourceprep str
   else 
      ""
