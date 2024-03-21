{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  makeFontsConf,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "modernx";
  version = "0.2.8";
  src = fetchFromGitHub {
    owner = "zydezu";
    repo = "ModernX";
    rev = finalAttrs.version;
    sha256 = "sha256-rruscDutFyQCl5sTtQfmtYPcXKWU51/QFbghSOyMC9o=";
  };

  postPatch = ''
    substituteInPlace modernx.lua \
      --replace "mp.find_config_file('scripts')" "\"$out/share/mpv/scripts\""
  '';

  dontBuild = true;
  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/mpv/scripts/
    mkdir -p $out/share/fonts/
    cp -r *.lua $out/share/mpv/scripts/
    cp -r *.ttf $out/share/fonts
    runHook postInstall
  '';
  passthru.scriptName = "modernx.lua";

  # the script uses custom "texture" fonts as the background for ui elements.
  # In order for mpv to find them, we need to adjust the fontconfig search path.
  passthru.extraWrapperArgs = [
    "--set"
    "FONTCONFIG_FILE"
    (toString (makeFontsConf {
      fontDirectories = ["${finalAttrs.finalPackage}/share/fonts"];
    }))
  ];

  meta = with lib; {
    description = "";
    homepage = "https://github.com/zydezu/ModernX";
    license = licenses.unlicense;
    maintainers = with lib.maintainers; [apfelkuchen6];
  };
})
