open Printf
  
let _ =
  let str = Sys.argv.(1) in
    printf "original: [%s]\n" str;
  let decoded = Punycode.decode str in
    printf "decoded: [%s]\n" (UTF8.encode decoded);
    let encoded = Punycode.encode (Array.of_list decoded) in
      printf "encoded:  [%s]\n" encoded;
      if str <> encoded then
        failwith("FAIL")
          
