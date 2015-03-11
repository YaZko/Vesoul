with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "vesoul";

  buildInputs = with ocamlPackages; [ ocaml ];
}
