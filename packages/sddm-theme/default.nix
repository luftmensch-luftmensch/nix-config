{stdenv, ...}:
# For some fucking reason SDDM does not support full paths -> Hence I had to symlink the background folder to the actual folder on my ~ directory
stdenv.mkDerivation {
  pname = "clairvoyance";
  version = "final";
  dontBuild = true;
  installPhase = ''
    mkdir -p $out/share/sddm/themes
    cp -aR $src $out/share/sddm/themes/clairvoyance
  '';
  src = ./.;

  meta.description = "Custom sddm theme";
}
