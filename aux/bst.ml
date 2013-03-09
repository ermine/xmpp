let make_bst a intv =
  let bst_height = ceil (log (float (Array.length a + 1)) /. log 2.0) in
  let bst_len = int_of_float (2. ** bst_height -. 1.0) in
  let bst = Array.make bst_len intv in
  let rec to_bst j s e =
    if s > e then ()
    else
      let mid = s + (e - s) / 2 in
        bst.(j-1) <- a.(mid);
        to_bst (j*2) s (mid-1);
        to_bst (j*2+1) (mid+1) e
  in
    match a with
      | [| _ |] -> a
      | _ -> to_bst 1 0 (Array.length a - 1);
        bst
  
