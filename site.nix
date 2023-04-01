{ pkgs, dependencies }: with pkgs; 
rec {
  site = (haskellPackages.callCabal2nix "site" "${./website}" {});

  site-with-dependencies = symlinkJoin {
      name = "site-with-dependencies";
      paths = [ site ];
      buildInputs = [ makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/website --set DEPENDENCIES "${linkFarm "dependencies" dependencies}"
      '';
    };

  commands = {
    rebuild = writeShellScriptBin "rebuild" ''
      cd ${site-with-dependencies}/bin
      ./website rebuild
    '';

    clean = writeShellScriptBin "clean" ''
      cd ${site-with-dependencies}/bin
      ./website clean
    '';

    watch = writeShellScriptBin "watch" ''
      cd ${site-with-dependencies}/bin
      ./website watch
    '';
  };
}
