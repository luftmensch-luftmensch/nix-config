{
  lib,
  buildGoModule,
  fetchFromGitHub,
  ...
}:
let
  version = "1.3.0";
in
buildGoModule {
  pname = "up";
  inherit version;

  src = fetchFromGitHub {
    owner = "jesusprubio";
    repo = "up";
    rev = "v${version}";
    hash = "sha256-fXg+fU7wCirJ1pGpahCjeYVUQukmTlnorS2F5eCcXCI=";
  };

  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
  ];

  vendorHash = "sha256-/Gsqc8rEptMBItqeb/N/gE4V3iUGZa8k1GqUR1+togY=";

  checkFlags = [ "-skip=^TestDNSProbe$" ];

  meta = {
    description = "Troubleshoot problems with your Internet connection";
    changelog = "https://github.com/jesusprubio/up/releases/";
    homepage = "https://github.com/jesusprubio/up";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ luftmensch-luftmensch ];
    mainProgram = "up-go";
  };
}
