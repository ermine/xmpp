(*
 * (c) 2008 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *
 * RFC 4648  The Base16, Base32, and Base64 Data Encodings
 *)

exception Bad_encoding

let base64chars = [|
   'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M';
   'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z';
   'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
   'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';
   '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';'8'; '9'; '+'; '/'
|]

let encode str =
   let c = String.length str mod 3 in
   let result = String.create 
      (String.length str / 3 * 4 + (if c > 0 then 4 else 0)) in
   let aux_encode i c1 c2 c3 =
      let n = c1 lsl 16 + c2 lsl 8 + c3 in
	 result.[i] <- base64chars.(n lsr 18 land 63);
	 result.[i+1] <- base64chars.(n lsr 12 land 63);
	 result.[i+2] <- base64chars.(n lsr 6 land 63);
	 result.[i+3] <- base64chars.(n land 63);
   in
   let z = String.length str - c in
   let rec aux_for i j =
      if i < z then (
	 aux_encode j
	    (Char.code str.[i]) (Char.code str.[i+1]) (Char.code str.[i+2]);
	 aux_for (i+3) (j+4)
      )
      else if c = 1 then
	 aux_encode j (Char.code str.[i]) 0x0 0x0
      else if c = 2 then
	 aux_encode j (Char.code str.[i]) (Char.code str.[i+1]) 0x0
      else
	 ()
   in
      aux_for 0 0;
      if c > 0 then
	 String.fill result (String.length result - (3-c)) (3-c) '=';
      result

let base64inv c =
   match c with
      | 'A' .. 'Z' -> Char.code c - 65
      | 'a' .. 'z' -> Char.code c - 97 + 26
      | '0' .. '9' -> Char.code c - 48 + 52
      | '+' -> 62
      | '/' -> 63
      | ' ' | '\t' | '\n' | '\r' -> -1
      | _   -> raise Bad_encoding

let decode str =
   let len = String.length str in
   let zlen, padding = 
      if str.[len - 1] = '=' then
	 if str.[len - 2] = '=' then len-2, 2 else len-1, 1
      else
	 len, 4 - (len mod 4)
   in
   let result = String.create ((zlen + padding) / 4 * 3 + (3-padding)) in
   let cap = Array.create 4 0 in
   let add_char c i j =
      let n = base64inv c in
	 if n >= 0 then (
	    cap.(i) <- n;
	    if i = 3 then (
	       let n = (cap.(0) lsl 18) + (cap.(1) lsl 12) + 
		  (cap.(2) lsl 6) + cap.(3) in
		  result.[j] <- (Char.chr (n lsr 16 land 255));
		  result.[j+1] <- (Char.chr (n lsr 8 land 255));
		  result.[j+2] <- (Char.chr (n land 255));
		  0, j+3
	    )
	    else
	       i+1, j
	 )
	 else
	    i, j
   in
   let rec aux_for i j k =
      if i < zlen then
	 let k,j = add_char str.[i] k j in
	    aux_for (i+1) j k
      else if padding = 1 then
	 let _k, j = add_char 'A' k j in
	    j
      else if padding = 2 then
	 let k, j = add_char 'A' k j in
	 let _k, j = add_char 'A' k j in
	    j
      else
	 j
   in
   let used = aux_for 0 0 0 in
      if String.length result > used then
	 String.sub result 0 used
      else
	 result
