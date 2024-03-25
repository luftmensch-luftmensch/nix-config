{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "visualizer";
  version = "unstable-2024-03-25";
  src = fetchFromGitHub {
    owner = "mfcc64";
    repo = "mpv-scripts";
    rev = "b4246984ba6dc6820adef5c8bbf793af85c9ab8e";
    sha256 = "1fvi4f90pnsd880f852352d5d5sqhaq9nqiw36r3zvwnhg1k7mb4";
  };

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/mpv/scripts
    cp visualizer.lua $out/share/mpv/scripts
    runHook postInstall
  '';

  patches = [./visualizer.patch];
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
