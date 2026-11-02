{
  description = "static websites";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/48652e9d5aea46e555b3df87354280d4f29cd3a3";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, ... }@inputs:
    let
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; };
        in
        {
          packages = {
            codeDist = pkgs.stdenv.mkDerivation {
              name = "website-code";
              src = ./website-code/dist;
              installPhase = "cp -r . $out";
            };

            yapDist = pkgs.stdenv.mkDerivation {
              name = "website-yap";
              src = ./website-yap/dist;
              installPhase = "cp -r . $out";
            };

            default = self.packages.${system}.codeDist;
          };
        };
    in
    flake-utils.lib.eachDefaultSystem perSystem;
}
