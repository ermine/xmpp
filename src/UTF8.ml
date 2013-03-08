  
exception MalformedUTF8
exception MalformedUnicode
  
let utf8_width u =
  if u < 0x80 then 1
  else if u < 0x800 then 2
  else if u < 0x10000 then 3
  else if u < 0x110000 then 4
  else raise MalformedUTF8

let width_of_utf8 = function
  | '\000'..'\127' -> 1
  | '\192'..'\223' -> 2
  | '\224'..'\239' -> 3
  | '\240'..'\247' -> 4
  | _ -> -1

let decode_utf8 str i =
  let ch1 = String.unsafe_get str i in
    match ch1 with
      | '\000'..'\127' -> Char.code ch1
      | '\192'..'\223' ->
        let ch2 = String.unsafe_get str (i+1) in
        let n1 = Char.code ch1 in
        let n2 = Char.code ch2 in
          if (n2 lsr 6 != 0b10) then raise MalformedUTF8
          else
            ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f)
      | '\224'..'\239' ->
        let ch2 = String.unsafe_get str (i+1) in
        let ch3 = String.unsafe_get str (i+2) in
        let n1 = Char.code ch1
        and n2 = Char.code ch2
        and n3 = Char.code ch3 in
          if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then
            raise MalformedUTF8
          else
            let code = 
              ((n1 land 0x0f) lsl 12) lor
                ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
            in
              if (code >= 0xd800) && (code <= 0xdf00) then
                raise MalformedUTF8
              else code
      | '\240'..'\247' ->
        let ch2 = String.unsafe_get str (i+1) in
        let ch3 = String.unsafe_get str (i+2) in
        let ch4 = String.unsafe_get str (i+3) in
        let n1 = Char.code ch1
        and n2 = Char.code ch2
        and n3 = Char.code ch3
        and n4 = Char.code ch4 in
          if (n2 lsr 6 != 0b10) ||
            (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10) then
            raise MalformedUTF8
          else
            ((n1 land 0x07) lsl 18) lor
              ((n2 land 0x3f) lsl 12) lor
              ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f)
      | _ ->
        raise MalformedUTF8
            
let encode_unicode ucs4 =
  let bytes = 
    if ucs4 < 0x80 then
      [ucs4]
    else if ucs4 <= 0x7ff then
      [(0xc0 lor (ucs4 lsr 6)); (0x80 lor (ucs4 land 0x3f))]
    else if ucs4 <= 0xffff then (
      if (ucs4 >= 0xd800 & ucs4 < 0xe000) then 
        raise MalformedUnicode;
      [(0xe0 lor (ucs4 lsr 12));
       (0x80 lor ((ucs4 lsr 6) land 0x3f));
       (0x80 lor (ucs4 land 0x3f))
      ]
    )
    else if ucs4 <= 0x10ffff then
      [(0xf0 lor (ucs4 lsr 18));
       (0x80 lor ((ucs4 lsr 12) land 0x3f));
       (0x80 lor ((ucs4 lsr 6)  land 0x3f));
       (0x80 lor (ucs4 land 0x3f))]
    else 
      raise MalformedUnicode
  in
    List.map Char.chr bytes

let decode str =
  let len = String.length str in
  let rec aux_decode acc i =
    if i < len then
      let w = width_of_utf8 (String.unsafe_get str i) in
        if i+w-1 < len then
          aux_decode (decode_utf8 str i :: acc) (i+w)
        else
          raise MalformedUTF8
    else
      List.rev acc
  in
    aux_decode [] 0
      

let encode ustr =
  let len = List.fold_left (fun len x -> utf8_width x + len) 0 ustr in
  let str = String.create len in
  let _len =
    List.fold_left (fun i x ->
      List.fold_left (fun j c -> String.unsafe_set str j c; succ  j
      ) i (encode_unicode x)
    ) 0 ustr in
    str
