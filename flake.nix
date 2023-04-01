{
  description = "ailrk.com";
    
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem ( system: 
    let
      pkgs = import nixpkgs { inherit system; };
      js-deps = {
        bulma = rec
          { name = "bulma";
            pkg = (builtins.fetchTarball { 
              url = "https://github.com/jgthms/bulma/archive/refs/tags/0.9.4.tar.gz";
              sha256 = "0j12iv99xx3mvyqpqa8k3grhdyp8061b74k61g54cmdyfm3gqdmm";
            });
            path = "${pkg}/sass" ;
          };
        line-awesome = rec
          { name = "line-awesome";
            pkg = (builtins.fetchTarball {
              url = "https://github.com/icons8/line-awesome/archive/refs/tags/v1.2.1.tar.gz";
              sha256 = "1m69lzlvjdpldpqqxhh2ylgjc0sz119yq4mlcz12c314kgpiv90f";
            });
            path = "${pkg}/dist/line-awesome";
          };
        katex = rec
          { name = "katex";
            pkg = (builtins.fetchTarball {
              url = "https://github.com/KaTeX/KaTeX/releases/download/v0.16.4/katex.tar.gz";
              sha256 = "0pgajj8vqfvgws3wnn5bfy4h3w75n4dz8jvjsxcw8s80jnp0md8c";
            });
            path = "${pkg}";
          };
      };

      site = pkgs.callPackage ./site.nix {
        inherit pkgs;
        dependencies = with js-deps; [ 
          bulma
          line-awesome
          katex
        ];
      };
    in {
      packages = {
        inherit (site);
        default = site.site-with-dependencies;
      };

      devShell = pkgs.haskellPackages.shellFor {
        packages = p: [ site.site ];
        buildInputs = with pkgs.haskellPackages; site.site.buildInputs ++ [
          haskell-language-server
          cabal-install
          site.commands.rebuild
          site.commands.clean
          site.commands.watch
        ];
      };
    });
}
