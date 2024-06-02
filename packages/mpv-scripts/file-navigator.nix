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
    rev = "a67c8280a7711cfaa5871f55d53ddb017f6d7b4c";
    sha256 = "08800syb9x5d4k9pqylfsi313ggg1sf20cnf48fcgbn54l7sm4hx";
  };

  dontBuild = true;
  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/mpv/scripts/
    cp -v navigator.lua $out/share/mpv/scripts/
    runHook postInstall
  '';

  postPatch = ''
    substituteInPlace navigator.lua \
      --replace-fail "'/media/HDD2/music/music/'," "" \
      --replace-fail "'/media/HDD/users/anon/Downloads/'," "" \
      --replace-fail "'/home/anon/'," ""
  '';

  passthru.scriptName = "navigator.lua";

  meta = {
    description = "Navigate and open your local files in mpv";
    homepage = "https://github.com/jonniek/mpv-filenavigator";
    license = lib.licenses.unfree;
    maintainers = with lib.maintainers; [ luftmensch-luftmensch ];
  };
})
