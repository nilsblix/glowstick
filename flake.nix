{
    description = "Terminal prompt written in Ocaml";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
        flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { self, nixpkgs, flake-utils }:
        flake-utils.lib.eachDefaultSystem (system:
            let
                pkgs = import nixpkgs { inherit system; };
                ocamlPackages = pkgs.ocamlPackages;
                glowstick = ocamlPackages.buildDunePackage {
                    pname = "glowstick";
                    version = "0.1.0";
                    src = self;
                    # duneVersion = "3.10";
                };
            in
            {
                packages.default = glowstick;
                apps.default = {
                    type = "app";
                    program = "${glowstick}/bin/main";
                };
                devShells.default = pkgs.mkShell {
                    packages = [
                        pkgs.ocamlPackages.utop
                        pkgs.ocamlPackages.ocaml
                        pkgs.ocamlPackages.dune_3
                    ];
                };
            });
}
