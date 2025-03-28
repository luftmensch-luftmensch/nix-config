{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "M-x";
  version = "0-unstable-2025-03-10";

  src = fetchFromGitHub {
    owner = "Seme4eg";
    repo = "mpv-scripts";
    rev = "beee45c82e6cd7a77aeb2bbd51e364d5a2899614";
    hash = "sha256-VAWYP00FNrsdjpGdh5J8AnnZIVv9DqwEP4J4QuNFjmg=";
  };

  scriptPath = ".";
  dontBuild = true;
  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/mpv/scripts
    cp M-x.lua script-modules/extended-menu.lua $out/share/mpv/scripts
    runHook postInstall
  '';

  postPatch = ''
    substituteInPlace M-x.lua \
      --replace-fail "toggle_menu_binding = 't'" "toggle_menu_binding = 'Alt+x'" \
      --replace-fail 'package.path =' "" \
      --replace-fail 'mp.command_native({ "expand-path", "~~/script-modules/?.lua;" }) .. package.path' "" \
      --replace-fail 'require "extended-menu"' "dofile(\"$out/share/mpv/scripts/extended-menu.lua\")"
  '';

  passthru.scriptName = "M-x.lua";

  meta = {
    description = "A menu that shows all commands you have available, key bindings and commends (if present) and from which you can call any of those commands";
    homepage = "https://github.com/Seme4eg/mpv-scripts/";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ luftmensch-luftmensch ];
  };
})
