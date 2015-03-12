with import <nixpkgs> {};

let
  ocaml-fp = pkgs.stdenv.lib.overrideDerivation
    pkgs.ocamlPackages_4_01_0.ocaml (old: {
    name = "ocaml-fp-4.01.0";
    configureFlags = old.configureFlags ++ [ "-with-frame-pointers" ];
  });
  ocamlPackages-fp = mkOcamlPackages ocaml-fp ocamlPackages-fp;
in

stdenv.mkDerivation {
  name = "vesoul";

  buildInputs = with ocamlPackages-fp; [ ocaml ];
}
