{
  lib,
  stdenv,
  makeWrapper,
  python3,
  playwright-driver,
}:
let
  pythonEnv = python3.withPackages (ps: [
    ps.argcomplete
    ps.playwright
  ]);
in
stdenv.mkDerivation {
  pname = "anisaturn";
  version = "0.1.0";

  dontUnpack = true;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/share/anisaturn
    cp ${./scraper.py} $out/share/anisaturn/scraper.py

    makeWrapper ${pythonEnv}/bin/python3 $out/bin/anisaturn \
      --add-flags "$out/share/anisaturn/scraper.py" \
      --set PLAYWRIGHT_BROWSERS_PATH ${playwright-driver.browsers} \

    runHook postInstall
  '';

  postInstall = ''
    # I don't really care for zsh tab completions as I currently
    # don't use zsh on my devices
    mkdir -p $out/share/bash-completion/completions
    ${pythonEnv}/bin/register-python-argcomplete anisaturn \
      > $out/share/bash-completion/completions/anisaturn

    # fish looks for "vendor" completions shipped by packages under
    # share/fish/vendor_completions.d/<name>.fish and loads them automatically.
    mkdir -p $out/share/fish/vendor_completions.d
    ${pythonEnv}/bin/register-python-argcomplete --shell fish anisaturn \
      > $out/share/fish/vendor_completions.d/anisaturn.fish
  '';

  meta = {
    description = "AnimeSaturn series urls scraper w/ Playwright";
    mainProgram = "anisaturn";
    platforms = lib.platforms.linux;
    maintainers = [ lib.maintainers.lufthmensch-luftmensch ];
  };
}
