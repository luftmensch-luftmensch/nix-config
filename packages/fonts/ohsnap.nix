{
  stdenvNoCC,
  fetchurl,
}:
let
  pname = "ohsnap";
  version = "1.8.0";
in
stdenvNoCC.mkDerivation {
  inherit pname version;

  src = fetchurl {
    url = "http://sourceforge.net/projects/osnapfont/files/${pname}-${version}.tar.gz";
    hash = "";
  };

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/fonts/truetype
    install -Dm644 *.otf $out/share/fonts/truetype
    runHook postInstall
  '';

  meta = {
    description = "Monospaced font based on Artwiz Snap";
    homepage = "http://sourceforge.net/projects/osnapfont/";
  };
}
