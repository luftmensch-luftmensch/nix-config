{
  lib,
  pkgs,
  stdenvNoCC,
  fetchFromGitHub,
  theme ? "default",
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "silent";
  version = "1.4.2";

  src = fetchFromGitHub {
    owner = "uiriansan";
    repo = "SilentSDDM";
    rev = "v${finalAttrs.version}";
    hash = "sha256-WeoJBj/PhqFCCJEIycTipqPbKm5BpQT2uzFTYcYZ30I=";
  };

  propagatedBuildInputs = with pkgs.kdePackages; [
    qtmultimedia
    qtsvg
    qtvirtualkeyboard
  ];

  dontWrapQtApps = true;

  installPhase =
    let
      basePath = "$out/share/sddm/themes/silent";
    in
    ''
      mkdir -p ${basePath}
      cp -r $src/* ${basePath}

      substituteInPlace ${basePath}/metadata.desktop \
        --replace-warn configs/default.conf configs/${theme}.conf

      chmod +w ${basePath}/configs/${theme}.conf

      chmod -R +w ${basePath}/backgrounds
    '';

  meta = {
    homepage = "https://github.com/uiriansan/SilentSDDM";
    description = "A very customizable SDDM theme that actually looks good";
    maintainers = [ lib.maintainers.luftmensch-luftmensch ];
    license = [ lib.licenses.gpl3 ];
  };
})
