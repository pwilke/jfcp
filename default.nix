with import <nixpkgs> {};

stdenv.mkDerivation {

  name = "jfcp";
  src = ./.;

  buildInputs = with ocamlPackages_4_01_0; [
    ocaml findlib
    cmdliner jsonm
  ];

}
