with import <nixpkgs> {};
with pkgs.ocaml-ng.ocamlPackages_4_12;

mkShell {
  buildInputs = [
    ocaml
    dune_2
    reason
    findlib
    ppxlib
    alcotest
    merlin
    ocaml-lsp
    nodejs
    yarn
    darwin.apple_sdk.frameworks.CoreServices
  ];
}
