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
  patches = [./navigator.patch];

  postPatch = ''
    substituteInPlace navigator.lua \
      --replace "mp.find_config_file('scripts')" "\"$out/share/mpv/scripts\""
  '';

  dontBuild = true;
  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/mpv/scripts/
    cp -v navigator.lua $out/share/mpv/scripts/
    runHook postInstall
  '';
  passthru.scriptName = "navigator.lua";

  meta = with lib; {
    description = " Navigate and open your local files in mpv";
    homepage = "https://github.com/jonniek/mpv-filenavigator";
    license = licenses.unlicense;
    maintainers = with lib.maintainers; [luftmensch-luftmensch];
  };
})
