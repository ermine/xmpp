open Stringprep

let rtest = Str.regexp
      "\\([0-9A-F ]+\\);\\([0-9A-F ]+\\);\\([0-9A-F ]+\\);\\([0-9A-F ]+\\);\\([0-9A-F ]+\\);\\(.*\\)"
let s = Str.regexp " "

let test () =
   let tst = open_in "NormalizationTest-3.2.0.txt" in
   let counter = ref 0 in
   let rec proc () =
      let line = input_line tst in
         if Str.string_match rtest line 0 then begin
            let c1 = (Str.matched_group 1 line) in
            let c2 = (Str.matched_group 2 line) in
            let c3 = (Str.matched_group 3 line) in
            let c4 = (Str.matched_group 4 line) in
            let c5 = (Str.matched_group 5 line) in
            let comment = Str.matched_group 6 line in

	    let split str =
	       let buffer = Buffer.create (String.length str * 3) in
		  List.iter (fun s -> 
				Utf8.store buffer (int_of_string ("0x" ^ s)))
		     (Str.split s str);
		  Buffer.contents buffer
	    in

	    let c1s = split c1 in
	    let c2s = split c2 in
	    let c3s = split c3 in
	    let c4s = split c4 in
	    let c5s = split c5 in
	       
	       Printf.printf "\n\nLine %d\n%s\n%s\n" !counter c4 c1;
	       if c4s <> stringprep c1s ||
		  c4s <> stringprep c2s ||
		  c4s <> stringprep c3s ||
		  c4s <> stringprep c4s ||
		  c4s <> stringprep c5s then begin
(*
		     let print ulist =
			String.concat " " 
			   (List.map (fun i -> Printf.sprintf "%X" i) 
			       (stringprep ulist))
		     in
			print_endline line;
			Printf.printf "need: %s\n" c4;
			Printf.printf "c1: %s\n" (print c1s);
			Printf.printf "c2: %s\n" (print c2s);
			Printf.printf "c3: %s\n" (print c3s);
			Printf.printf "c4: %s\n" (print c4s);
			Printf.printf "c5: %s\n" (print c5s);
			failwith "stop."
*)
		     ()
		  end
	 end;
	 incr counter;
         proc ()
   in
      try
         proc ()
      with End_of_file ->
         close_in tst

let _ =
   test ()
