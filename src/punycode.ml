(* RFC 3492 *)

exception Error
exception Overflow

let base         = 36
let tmin         = 1
let tmax         = 26
let skew         = 38
let damp         = 700
let initial_bias = 72
let initial_n    = 128
let delimiter = 0x2D

let is_basic cp = cp < 0x80

let decode_digit cp =
  if cp - 48 < 10 then
    cp - 22
  else if cp - 65 < 26 then
    cp - 65
  else if cp - 97 < 26 then
    cp - 97
  else
    raise Error

let encode_digit d =
  d + 22 + 75 * (if d < 26 then 1 else 0)
      
let adapt_bias delta numpoints firsttime =
  let delta = if firsttime then delta / damp else delta asr 1 in
  let delta = delta + (delta / numpoints) in
  let rec aux_while k delta =
    if delta > ((base - tmin) * tmax) asr 1 then
      aux_while (k+base) (delta / (base - tmin))
    else
      k, delta
  in
  let k, delta = aux_while 0 delta in
    k + (((base - tmin + 1) * delta) / (delta + skew))
    
let insert i n outstr =
  let rec aux_insert j = function
    | [] -> [n]
    | x :: xs ->
      if j = i then
        n ::x::xs
      else
        x::aux_insert (succ j) xs
  in
    aux_insert 0 outstr
        

let decode instr =
  let instr_len = String.length instr in
  let n = initial_n in
  let i = 0 in
  let bias = initial_bias in
  let idx =
    try String.rindex instr '-'
    with _ -> 0 in
  let idx, outstr =
    if idx = 0 then
      idx, []
    else
      let rec aux_copy j outstr =
        if j < idx then
          if is_basic (Char.code instr.[j]) then
            aux_copy (succ j) ((Char.code instr.[j]) :: outstr)
          else
            raise Error
        else
          List.rev outstr
      in
        succ idx, aux_copy 0 [] in
  let rec aux_decode idx i bias n outstr outstr_len =
    if idx < instr_len then (
      let oldi = i in
      let rec aux k i w idx =
        if idx >= instr_len then raise Error;
        let digit = decode_digit (Char.code instr.[idx]) in
        let i = i + digit * w in (* TODO: check overflow *)
        let t =
          if k <= bias then tmin
          else if k >= bias + tmax then tmax else k - bias in
          if digit < t then
            (oldi, i, idx) 
          else
            let w = w * (base - t) in (* TODO: check overflow *)
              aux (k + base) i w (succ idx)
      in
      let oldi, i, idx = aux base i 1 idx in
      let bias = adapt_bias (i - oldi) (outstr_len + 1) (oldi = 0) in
      let n = n + i / (outstr_len + 1) in (* TODO: check overflow *)
      let i = i mod (List.length outstr + 1) in
        if is_basic n then raise Error;
        let outstr = insert i n outstr in
          aux_decode (succ idx) (succ i) bias n outstr (succ outstr_len)
    ) else
      outstr
  in
    aux_decode idx i bias n outstr (List.length outstr)
          
let encode instr =
  let instr_len = Array.length instr in
  let n = initial_n in
  let delta = 0 in
  let bias = initial_bias in

  let rec aux_collect_bcp j outstr =
    if j < instr_len then
      if is_basic instr.(j) then
        aux_collect_bcp (succ j) (instr.(j) :: outstr)
      else
        aux_collect_bcp (succ j) outstr
    else
      outstr
  in
  let outstr = aux_collect_bcp 0 [] in
  let b = List.length outstr in
  let h = b in
  let outstr = if b > 0 then delimiter :: outstr else outstr in

  let rec aux_encode h n bias delta outstr =
    if h < instr_len then
      let rec get_min j m =
        if j < instr_len then
          if instr.(j) >= n && instr.(j) < m then get_min (succ j) instr.(j)
          else get_min (succ j) m
        else
          m
      in
      let m = get_min 0 0x10FFFF in
      let delta = delta + (m - n) * (h + 1) in (* TODO: check overflow *)
      let n = m in
      let rec aux_foreach idx h delta bias outstr =
        if idx < instr_len then 
          if instr.(idx) < n then
            (* TODO: check overflow for delta + 1 *)
            aux_foreach (succ idx) h (succ delta) bias outstr
          else if instr.(idx) = n then
              let rec aux_for k q outstr =
                let t = if k <= bias then tmin
                    else if k >= bias + tmax then tmax else k - bias in
                  if q < t then
                    q, outstr
                  else
                    let o = encode_digit (t + ((q - t) mod (base - t))) in
                    let q = (q - t) / (base - t) in
                      aux_for (k + base) q (o::outstr)
              in
              let q, outstr = aux_for base delta outstr in
              let outstr = encode_digit q :: outstr in
              let bias = adapt_bias delta (succ h) (h = b) in
              let delta = 0 in
                aux_foreach (succ idx) (succ h) delta bias outstr
          else
            aux_foreach (succ idx) h delta bias outstr
          else
            h, delta, bias, outstr
      in
      let h, delta, bias, outstr = aux_foreach 0 h delta bias outstr in
        aux_encode h (succ n) bias (succ delta) outstr
    else
      List.rev outstr
  in
  let outstr = aux_encode h n bias delta outstr in
  let len = List.length outstr in
  let str = String.create len in
  let _ = List.fold_left (fun i c -> str.[i] <- Char.chr c; succ i) 0 outstr in
    str
                                    
    
