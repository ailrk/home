{
  description = "website";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/48652e9d5aea46e555b3df87354280d4f29cd3a3";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, ... }@inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              website = (hfinal.callPackage ./default.nix {});
            };
        };
        website =
          with final.haskell.lib.compose;
          overrideCabal (drv: {
            disallowGhcReference = false;
            enableSeparateDataOutput = false;
          }) (justStaticExecutables final.haskellPackages.website);
      };

      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShells = {
            default = pkgs.callPackage ./shell.nix { hspkgs = hspkgs; };
          };

          packages = rec {
            default = website;
            website = pkgs.website;
          };
        };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem perSystem;
}
