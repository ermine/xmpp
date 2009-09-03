open Ocamlbuild_plugin
open Myocamlbuild_config

let _ =  dispatch begin function
  | After_rules ->
      extern "xml";
      extern "mltls";
      extern "treap";
      
      install_lib "xmpp" ~cma:"xMPP" []
        
  | _ ->
      ()
end
