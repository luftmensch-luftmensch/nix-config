{
  lib,
  stdenvNoCC,
  fetchgit,
}:
stdenvNoCC.mkDerivation rec {
  pname = "icomoon-feather-font";
  version = "2023-05-06";

  # 参考 https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=ttf-icomoon-feather
  src = fetchgit {
    url = "https://github.com/adi1090x/polybar-themes.git";
    rev = "47b66337a92a1afd2240ed7094ffcb039cc686cf"; # git commit id
    sparseCheckout = ["fonts/feather.ttf"]; # only fetch the feather.ttf file
    sha256 = "sha256-R+UpUFkXDrxKcX7ljLara+1B1rOMdKGZiLQq1/ojgP4=";
  };

  installPhase = ''
    runHook preInstall

    install -Dm644 fonts/feather.ttf -t $out/share/fonts/truetype/

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://github.com/feathericons/feather";
    description = "Icomoon feather font";
    inherit version;
    longDescription = ''
      Feather is a collection of simply beautiful open source icons.
      Each icon is designed on a 24x24 grid with an emphasis on simplicity, consistency, and flexibility.
    '';
    license = licenses.mit;
    maintainers = [maintainers.ryan4yin];
    platforms = platforms.all;
  };
}
