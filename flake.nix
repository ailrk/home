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
          hspkgs = pkgs.haskellPackages;
        in
        {
          packages = {
            websiteDist = pkgs.stdenv.mkDerivation {
              name = "website";
              src = ./dist;
              installPhase = "cp -r . $out";
            };

            default = self.packages.${system}.websiteDist;
          };

          devShells = rec {
            default = filehub-shell;
            filehub-shell = pkgs.callPackage ./shell.nix { inherit hspkgs; };
          };
        };
    in
    flake-utils.lib.eachDefaultSystem perSystem;
}
