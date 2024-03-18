{stdenv, ...}:
stdenv.mkDerivation {
  pname = "clairvoyance";
  version = "1.0";
  dontBuild = true;
  installPhase = ''
    mkdir -p $out/share/sddm/themes
    cp -aR $src $out/share/sddm/themes/clairvoyance
  '';
  src = ./.;

  meta.description = "Custom sddm theme";
}
