{ pkgs, hspkgs }:
hspkgs.shellFor {
  withHoogle = true;
  packages = p: [];
  buildInputs = [
    hspkgs.cabal-install
    hspkgs.haskell-language-server
    hspkgs.hlint
    hspkgs.cabal2nix
    pkgs.bashInteractive
  ];
}
