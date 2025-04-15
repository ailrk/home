{ mkDerivation, base, bytestring, directory, filepath, hakyll
, hakyll-images, hakyll-sass, lib, lucid, pandoc, text, time
}:
mkDerivation {
  pname = "website";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring directory filepath hakyll hakyll-images hakyll-sass
    lucid pandoc text time
  ];
  license = "unknown";
  mainProgram = "website";
}
