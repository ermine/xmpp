open Xmpp_prep_tables

exception Error of string
  
let rec get_bst arr x s e =
  if s > e || x < Array.unsafe_get (Array.unsafe_get arr s) 0 ||
    x > Array.unsafe_get (Array.unsafe_get arr e) 1 then
    -1
  else
    let mid = s + (e-s)/2 in
    let amid = Array.unsafe_get arr mid in
      if x < Array.unsafe_get amid 0 then
        get_bst arr x s (mid-1)
      else if x > Array.unsafe_get amid 1 then
        get_bst arr x (mid+1) e
      else
        Array.unsafe_get amid 2

let get_casemap x =
  if x > 0x40 && x <0x5b then
    Array.unsafe_get (Array.unsafe_get casemap 0) 2
  else
    get_bst casemap x 1 casemap_max_idx

let get_b1_b2 x =
  let v = get_casemap x in
    if v = -1 then
      [x]
    else if v = 0 then
      []
    else if v land b_mc = b_mc then
      Array.to_list (Array.unsafe_get fmap (v asr b_shift))
    else
      [x + (v asr b_shift)]

let get_b1 x =
  let v = get_casemap x in
    if v = 0 then
      -1
    else
      x
          

(* http://unicode.org/reports/tr15/ *)

let hangulSBase = 0xAC00
let hangulLBase = 0x1100
let hangulVBase = 0x1161
let hangulTBase = 0x11A7        
let hangulLCount = 19
let hangulVCount = 21
let hangulTCount = 28
let hangulNCount = hangulVCount * hangulTCount (* 588 *)
let hangulSCount = hangulLCount * hangulNCount (* 11172 *)

let get_decomp x =
  let rec get s e =
    if s > e then
      [x, 0]
    else
      let mid = s + (e - s) / 2 in
      let (r, a) = Array.unsafe_get dmap mid in
        if x < r then
          get s (mid-1)
        else if x > r + Array.length a - 1 then
          get (mid+1) e
        else
          let data = Array.unsafe_get a (x - r) in
            List.map (fun d ->
              (d lsr 8, d land 0xFF)) (Array.to_list data)
  in
    if x > 0x009F && x < 0x2FA1E then
      get 0 dmap_max_idx
    else
      [x, 0]

let compatibility_decompose x =        
  let sindex = x - hangulSBase in
    if sindex < 0 || sindex >= hangulSCount then
      get_decomp x
    else
      let l = hangulLBase + sindex / hangulNCount in
      let v = hangulVBase + (sindex mod hangulNCount) / hangulTCount in
      let t = hangulTBase + sindex mod hangulTCount in
        (l,0) :: (v,0) :: (if t <> hangulTBase then [t,0] else [])

let decompose ustr =
  let rec canonical_order acc prev_cc prev_rune = function
    | [] -> List.rev ((prev_rune, prev_cc) :: acc)
    | (xr, xc) :: xs ->
      if xc = 0 || prev_cc <= xc then
        canonical_order ((prev_rune, prev_cc) :: acc) xc xr xs
      else
        match acc with
          | [] -> canonical_order [xr, xc] prev_cc prev_rune xs
          | (ar, ac) :: atl ->
            canonical_order atl ac ar ((xr, xc) :: (prev_rune, prev_cc) :: xs)
  in
  let result =
    List.fold_left (fun acc x ->
      let ds = compatibility_decompose x in
        (List.fold_left (fun acc d -> d :: acc) acc ds)
    )  [] ustr in
  let result = List.rev result in
    match result with
      | [] -> []
      | (xr, xc) :: xs -> canonical_order [] xc xr xs
              
let compose_hangul ch1 ch2 =
  (* check if two current characters are L and V *)
  let lindex = ch1 - hangulLBase in
  let vindex = ch2 - hangulVBase in
    if (lindex >= 0 && lindex < hangulLCount) &&
      (vindex >= 0 && vindex < hangulVBase) then
      (* make syllable of form LV *)
      hangulSBase + (lindex * hangulVCount + vindex) * hangulTCount
    else
      (* 2. check to see if two current characters are LV and T *)
      let sindex = ch1 - hangulSBase in
      let tindex = ch2 - hangulTBase in
        if (sindex >= 0 && sindex < hangulSCount &&
              (sindex mod hangulTCount) == 0) &&
          (tindex > 0 && tindex < hangulTCount) then
          (* make syllable of form LVT *)
          ch1 + tindex
        else
          (* if neither case was true *)
          -1

let comp_len = Array.length comp_map

let composeTwo ch1 ch2 =
  let rec check_ch1 branch s e =
    if s > e then
      -1
    else
      let mid = s + (e-s) / 2 in
      let amid = Array.unsafe_get branch mid in
        if fst amid = ch1 then
          snd amid
        else if ch1 < fst amid then
          check_ch1 branch s (mid-1)
        else
          check_ch1 branch (mid+1) e
  in
  let rec get s e =
    if s > e then
      -1
    else
      let mid = s + (e - s) / 2 in
      let amid = Array.unsafe_get comp_map mid in
        if fst amid = ch2 then
          match snd amid with
            | [| (x1, x2) |] -> if ch1 = x1 then x2 else -1
            | branch -> check_ch1 branch 0 (Array.length branch - 1)
        else if ch2 < fst amid then
          get s (mid-1)
        else
          get (mid+1) e
  in
  let newch = compose_hangul ch1 ch2 in
    if newch = -1 && (ch2 > 767 && ch2 < 12443) then
      get 0 comps_max_idx
    else
      newch
    
let compose ulist =
  let rec aux_iter acc comps ch1 prevcc = function
    | [] ->
      List.rev (comps @ (ch1 :: acc))
    | (ch2, cc) :: xs ->
      let newch1 =
        if prevcc = 0 || cc > prevcc then
          composeTwo ch1 ch2
        else
          -1
      in
        if newch1 > -1 then
          aux_iter acc comps newch1 prevcc xs
        else if cc = 0 then
          let acc = comps @ ch1 :: acc in
            aux_iter  acc [] ch2 0 xs
        else
          aux_iter acc (ch2 :: comps) ch1 cc xs
  in
    match ulist with
      | [] -> []
      | (ch1, cc) :: cs ->
        aux_iter [] [] ch1 cc cs

let nfkc ustr =
  compose (decompose ustr)

let nodeprep ustr =
  let result =
    List.fold_left (fun acc x ->
      match get_b1_b2 x with
        | [] -> acc
        | [x] -> x::acc
        | r -> List.rev r @ acc
    ) [] ustr in
    nfkc (List.rev result)
        

let resourceprep ustr =
  let result =
    List.fold_left (fun acc x ->
      if get_b1 x = -1 then
        acc
      else
        x::acc
    ) [] ustr in
    nfkc (List.rev result)
  

let prohibits_max_idx = Array.length prohibits - 1
  
(* Check prohibited symbols and bidi *)
let check_prohibits p ustr =
  let rec aux_prohibit dir = function
    | [] -> ()
    | x :: xs ->
      let v = get_bst prohibits x 0 prohibits_max_idx in
        if v = -1 then
          if xs = [] && dir = b_randal then
            raise (Error "invalid bidi")
          else
            aux_prohibit dir xs
        else if v land p = p then
          raise (Error "prohibited symbol")
        else if v land dir != dir &&
               (v land b_randal = b_randal || v land b_l = b_l) then
          raise (Error "invalid bidi")
        else
          aux_prohibit dir xs
  in
    match ustr with
      | [] -> ()
      | x :: xs ->
        let v = get_bst prohibits x 0 prohibits_max_idx in
          if v = -1 then
            aux_prohibit b_l xs
          else if v land p = p then
            raise (Error "prohibited symbol")
          else if v land b_randal = b_randal then
            aux_prohibit b_randal xs
          else
            aux_prohibit b_l xs

let strong_nodeprep str =
  let normalized = nodeprep str in
    check_prohibits b_nodeprep_prohibit normalized;
    normalized

let strong_resourceprep str =
  let normalized = resourceprep str in
    check_prohibits b_resourceprep_prohibit normalized;
    normalized
    
(* for domains *)
let nameprep ustr =
  let result =
    List.fold_left (fun acc x ->
      match get_b1_b2 x with
        | [] -> acc
        | [x] -> x::acc
        | r -> List.rev r @ acc
    ) [] ustr in
    nfkc (List.rev result)

let strong_nameprep str =
  let normalized = nameprep str in
    check_prohibits b_nameprep_prohibit normalized;
    normalized
      
