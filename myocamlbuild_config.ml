open Ocamlbuild_plugin
open Ocamlbuild_pack.Ocamlbuild_where
open Command

let config =
  let split line =
    let sp = String.index line '=' in
      ((String.sub line 0 sp),
       (String.sub line (sp+1) (String.length line - sp - 1)))
  in
    if Pathname.exists "myocamlbuild.config" then
      List.map split (string_list_of_file "myocamlbuild.config")
    else
      []

let ocamlfind_query pkg =
  let cmd = Printf.sprintf
    "%s/ocamlfind query %s" !bindir (Filename.quote pkg) in
    Ocamlbuild_pack.My_unix.run_and_open cmd
      (fun ic ->
         (* Log.dprintf 5 "Getting Ocaml directory from command %s" cmd; *)
         input_line ic)

let tags =
  ["use_bigarray"; "use_dbm"; "use_dynlink"; "use_graphics";
   "use_nums"; "use_str"; "use_toplevel"; "use_unix"; "use_threads"]

let pkg_tags = ref []

let rec ocamlfind_get_deps predicates pkg =
  let rec aux_parse acc ic =
    let line = try Some (input_line ic) with _ -> None in
      match line with
        | None -> S (List.rev acc)
        | Some v ->
            let r = Ocamlbuild_pack.Lexers.space_sep_strings &
              Lexing.from_string v in
            let pkg, tail = List.hd r, List.tl r in
              if List.mem ("use_" ^ pkg) tags then
                aux_parse acc ic
              else if List.mem ("use_" ^ pkg, predicates) !pkg_tags then
                aux_parse acc ic
              else (
                pkg_tags := ("use_" ^ pkg, predicates) :: !pkg_tags;
                let deps = ocamlfind_get_deps predicates pkg in
                let rest = S[deps; A"-I"; A(List.hd tail);
                             S(List.map (fun a -> A a) (List.tl tail))]
                in
                  aux_parse (rest :: acc) ic
              )
  in
  let cmd = Printf.sprintf
    "%s/ocamlfind query -r -predicates %s -format \"%%p %%d %%A\" %s"
    !bindir predicates pkg in
    Ocamlbuild_pack.My_unix.run_and_open cmd &
      aux_parse []

let add_package pkg =
  let use_pkg = "use_" ^ pkg in
    flag ["ocaml"; "compile"; use_pkg] &
      S[A"-I"; A(ocamlfind_query pkg)];
    flag ["ocaml"; "link"; "native"; use_pkg] &
      ocamlfind_get_deps "native,mt" pkg;
    flag ["ocaml"; "link"; "byte"; use_pkg] &
      ocamlfind_get_deps "byte,mt" pkg

let extern ?cma ?tag_name name =
  try
    let path = List.assoc name config in
      ocaml_lib ~extern:true ?dir:(if path = "" then None else Some path)
        ?tag_name (match cma with | None -> name | Some name -> name)
  with Not_found ->
    add_package name
  
let make_binding ?include_dir ?lib_dir ~lib ?headers name =
  flag ["ocamlmklib"; "c"; ("use_" ^ name ^ "_clib")] &
    (match lib_dir with
       | None -> S[A lib]
       | Some dir -> S[A dir; A lib]
    );
               
  (match include_dir with
     | None -> ()
     | Some dir ->
         flag ["c"; "compile"; ("include_" ^ name ^ "_clib")] &
           S[A"-ccopt"; A dir]
  );

  flag ["link"; "ocaml"; "library"; ("use_" ^ name ^ "_clib")] &
    (match lib_dir with
       | None -> S[A"-cclib"; A lib]
       | Some dir -> S[A"-ccopt"; A dir; A"-cclib"; A lib]
    );
  
  (* If `static' is true then every ocaml link in bytecode will add -custom *)
  if true then
    flag ["link"; "ocaml"; "byte"; "use_lib" ^ name] & A"-custom";

  ocaml_lib name;
  
  flag ["link"; "library"; "ocaml"; "byte"; "use_lib" ^ name] &
    S[A"-dllib"; A("-l" ^ name);
      A"-ccopt"; A("-L ."); A"-cclib"; A("-l" ^ name)];

  flag ["link"; "library"; "ocaml"; "native"; "use_lib" ^ name] &
    S[A"-ccopt"; A"-L."; A"-cclib"; A("-l" ^ name)];

  dep  ["link"; "ocaml"; ("use_lib" ^ name)] ["lib" ^ name -.- "a"];
  
  (* This will import headers in the build directory. *)
  match headers with
    | None -> ()
    | Some h ->
        dep  ["compile"; "c"] h
  
let install_dir = "../../site-lib"
let bytecode = true
let nativecode = true

let make_deps name =
  if bytecode then
    name -.- "cma" :: (if nativecode then [name -.- "cmxa"] else [])
  else
    (if nativecode then [name -.- "cmxa"] else [])

let install_lib name ?cma modules =
  let cma =
    match cma with
      | None -> name
      | Some v -> v
  in
  let deps = make_deps cma in
    rule "Install"
      ~prod:"install"
      ~deps
      (fun env _build ->
         let deps = List.map (fun file -> A file) deps in
         let mllib =
           let mllib = cma -.- "mllib" in
             if Pathname.exists mllib then
               let l =
                 List.map String.uncapitalize (string_list_of_file mllib) in
                 List.fold_left (fun acc f ->
                                   if Pathname.exists (f -.- "mli") then
                                     A (f -.- "mli") :: A (f -.- "cmi") :: acc
                                   else
                                     A (f -.- "cmi") :: acc
                                ) [A (cma -.- "a")] l
             else if Pathname.exists (cma -.- "mli") then
               [A (cma -.- "mli") ; A (cma -.- "cmi")]
             else
               [A (cma -.- "cmi")]
         in
         let files = List.map (fun file -> A file) modules in
           Seq [Cmd (S[A"mkdir"; A"-p"; P (install_dir / name)]);
                Cmd (S[Px"install"; S deps; P (install_dir / name)]);
                Cmd (S[Px"install"; S mllib; S files; P (install_dir / name)]);
               ]
      )
