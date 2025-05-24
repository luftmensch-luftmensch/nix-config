{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  gtk3,
  adwaita-icon-theme,
  # kdePackages,
  hicolor-icon-theme,
}:
let
  pname = "haiku-icon-theme";
  version = "0-unstable-2024-12-24";
in
stdenvNoCC.mkDerivation {
  inherit pname version;

  src = fetchFromGitHub {
    owner = "tallero";
    repo = "haiku-icon-theme";
    rev = "7577be42c717faace2c5a4db5cc92850d76df42b";
    hash = "sha256-3CRz0zRwDtZF1MgPG1+xyiimxG4RLZshW92hjk+TjRA=";
  };

  nativeBuildInputs = [
    gtk3
  ];

  propagatedBuildInputs = [
    adwaita-icon-theme
    # kdePackages.breeze-icons
    hicolor-icon-theme
  ];

  dontDropIconThemeCache = true;
  # dontWrapQtApps = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/icons
    cp -a Haiku $out/share/icons/

    for theme in $out/share/icons/*; do
      gtk-update-icon-cache -f $theme
    done

    runHook postInstall
  '';

  meta = {
    description = "Haiku icon theme";
    homepage = "https://github.com/tallero/haiku-icon-theme";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
    maintainers = [ lib.maintainers.luftmensch-luftmensch ];
  };
}
