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
    mkfontdir "$out/share/fonts"
    install -m 644 -D *.pcf -t "$out/share/fonts"
    install -m 644 -D *.psfu -t "$out/share/kbd/consolefonts"
    runHook postInstall
  '';

  meta = {
    description = "Monospaced font based on Artwiz Snap";
    homepage = "http://sourceforge.net/projects/osnapfont/";
  };
}
