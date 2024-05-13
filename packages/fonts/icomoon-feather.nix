{
  lib,
  stdenvNoCC,
  fetchgit,
}:
stdenvNoCC.mkDerivation rec {
  pname = "icomoon-feather";

  version = "2024-05-11";

  src = fetchgit {
    url = "https://github.com/adi1090x/polybar-themes.git";
    rev = "adb6a4546a8351a469fa779df173e46b69aa1ac3";
    sparseCheckout = ["fonts/panels/icomoon_feather.ttf"];
    sha256 = "sha256-QL7/pfIqOd2JOm6rkH+P4rMg0AhGllfkReQ03YeGW+8=";
  };

  installPhase = ''
    runHook preInstall

    install -Dm644 fonts/panels/icomoon_feather.ttf -t $out/share/fonts/truetype/

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://github.com/adi1090x/polybar-themes/tree/master/fonts/panels";
    description = "Icomoon feather font";
    inherit version;
    license = licenses.agpl3Plus;
    maintainers = with maintainers; [luftmensch-luftmensch];
    platforms = platforms.all;
  };
}
