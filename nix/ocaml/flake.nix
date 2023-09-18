{
    description = "OCaml 4.12 packages for ReScript PPX development";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
        flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { self, nixpkgs, flake-utils }:
        flake-utils.lib.eachDefaultSystem (system:
            let
                overlay = (final: prev: {
                    ocaml-ng = prev.ocaml-ng // {
                        ocamlPackages_4_12 = prev.ocaml-ng.ocamlPackages_4_12 // {
                            ppxlib = prev.ocaml-ng.ocamlPackages_4_12.ppxlib.overrideAttrs (_: {
                                version = "git";
                                src = pkgs.fetchFromGitHub {
                                    owner = "zth";
                                    repo = "ppxlib";
                                    rev = "32f83395fb89693a873541298b6367449f23bc4a";
                                    sha256 = "sha256-8bkmeFh5Unda8n3F2MQWi81QPt2NdkwFcy4wZTJ0STo=";
                                };
                                patches = [];
                            });
                        };
                    };
                });

                pkgs = import nixpkgs {
                    inherit system;
                    overlays = [ overlay ];
                };

                ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_12;
            in
            {
                packages = {
                    ocaml = ocamlPackages.ocaml;
                    dune = ocamlPackages.dune_3;
                    reason = ocamlPackages.reason;
                    result = ocamlPackages.result;
                    findlib = ocamlPackages.findlib;
                    ppxlib = ocamlPackages.ppxlib;
                    alcotest = ocamlPackages.alcotest;
                    merlin = ocamlPackages.merlin;
                    lsp = ocamlPackages.ocaml-lsp;
                };
            }
        );
}
