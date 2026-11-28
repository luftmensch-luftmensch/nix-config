{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  ...
}:
let
  pname = "firefox-parfait";
  version = "1.3";
in
stdenvNoCC.mkDerivation {
  inherit pname version;

  src = fetchFromGitHub {
    owner = "reizumii";
    repo = "parfait";
    rev = "v${version}";
    hash = "sha256-hd5u6ogZAzk5K1QZs/+wIzbBF6Ovpii/PW0+f/FhPIs=";
  };

  dontConfigure = true;
  dontBuild = true;
  installPhase = ''
    runHook preInstall

    install -Dm644 ./user{Chrome,Content}.css -t $out/share/firefox-parfait
    cp -r ./parfait $out/share/firefox-parfait

    runHook postInstall
  '';

  meta = {
    description = "A tasty theme modification for Firefox 🦊";
    changelog = "https://github.com/reizumii/parfait/releases";
    homepage = "https://github.com/reizumii/parfait";
    license = lib.licenses.mpl20;
    maintainers = [ lib.maintainers.luftmensch-luftmensch ];
    platforms = lib.platforms.all;
  };
}
