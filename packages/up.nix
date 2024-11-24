{
  lib,
  buildGoModule,
  fetchFromGitHub,
  ...
}:
let
  version = "1.1.0";
in
buildGoModule {
  pname = "up";
  inherit version;

  src = fetchFromGitHub {
    owner = "jesusprubio";
    repo = "up";
    rev = "v${version}";
    hash = "sha256-sr57l/yDCsc+PUCjtSrOKdtAxqObDJpO6zF6eWVKezc=";
  };

  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
  ];

  vendorHash = "sha256-/Gsqc8rEptMBItqeb/N/gE4V3iUGZa8k1GqUR1+togY=";

  checkFlags =
    let
      # Skip tests that require network access
      skippedTests = [
        "TestDNSProbe"
      ];
    in
    [ "-skip=^${builtins.concatStringsSep "$|^" skippedTests}$" ];

  meta = {
    description = "Troubleshoot problems with your Internet connection";
    changelog = "https://github.com/jesusprubio/up/releases/";
    homepage = "https://github.com/stefanlogue/meteor";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ luftmensch-luftmensch ];
    mainProgram = "up-go";
  };
}
