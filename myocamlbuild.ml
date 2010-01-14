open Ocamlbuild_plugin
open Myocamlbuild_config

let _ =  dispatch begin function
  | After_rules ->
      extern "xml";
      extern "mltls";
      extern "treap";
      extern "base64";
      extern "cryptokit";
      
      install_lib "xmpp" ~cma:"XMPP" []
        
  | _ ->
      ()
end
