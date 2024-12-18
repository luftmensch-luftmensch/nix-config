{
  lib,
  fetchFromGitHub,
  stdenvNoCC,
}:
stdenvNoCC.mkDerivation {
  pname = "sf-mono";
  version = "0-unstable-2023-07-02";

  src = fetchFromGitHub {
    owner = "shaunsingh";
    repo = "SFMono-Nerd-Font-Ligaturized";
    rev = "dc5a3e6fcc2e16ad476b7be3c3c17c2273b260ea";
    hash = "sha256-AYjKrVLISsJWXN6Cj74wXmbJtREkFDYOCRw1t2nVH2w=";
  };

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/fonts/opentype
    cp *.otf $out/share/fonts/opentype/
    runHook postInstall
  '';

  meta = {
    description = "Apple's SFMono font nerd-font patched and ligaturized";
    homepage = "https://github.com/shaunsingh/SFMono-Nerd-Font-Ligaturized";
    platforms = lib.platforms.all;
  };
}
