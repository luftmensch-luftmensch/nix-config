{
  lib,
  stdenvNoCC,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "visualizer";
  version = "1.0";
  # Taken from https://nix.dev/tutorials/working-with-local-files.html
  src = lib.fileset.toSource {
    root = ./.;
    fileset = ./visualizer.lua;
  };
  postInstall = ''
    mkdir -p $out/share/mpv/scripts/
    cp -v visualizer.lua $out/share/mpv/scripts/
  '';

  passthru.scriptName = "visualizer.lua";

  meta = with lib; {
    description = "Visualizer for audio files";
    homepage = "https://github.com/mfcc64/mpv-scripts";
    license = licenses.unlicense;
    maintainers = with lib.maintainers; [luftmensch-luftmensch];
  };
})
