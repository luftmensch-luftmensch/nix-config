{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "mpv-filenavigator";
  version = "1.0";
  src = fetchFromGitHub {
    owner = "jonniek";
    repo = finalAttrs.pname;
    rev = "51242195da9b3231ab7fde367a63dc58fb6858f3";
    hash = "sha256-JjYDBdoPcNH+SVbOIFICJSM1sH6t6IEA2yHnHMbHpV8=";
  };

  dontBuild = true;
  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/mpv/scripts/
    cp -v navigator.lua $out/share/mpv/scripts/
    runHook postInstall
  '';

  passthru.scriptName = "navigator.lua";

  meta = {
    description = "Navigate and open your local files in mpv";
    homepage = "https://github.com/jonniek/mpv-filenavigator";
    license = lib.licenses.unlicense;
    maintainers = with lib.maintainers; [ luftmensch-luftmensch ];
  };
})
