with import <nixpkgs> {};
with pkgs.ocaml-ng.ocamlPackages_4_12;

mkShell {
  buildInputs = [
    ocaml
    dune_3
    reason
    result
    findlib
    ppxlib
    alcotest
    merlin
    ocaml-lsp
    nodejs
    yarn
    awscli2
    darwin.apple_sdk.frameworks.CoreServices
  ];
}
