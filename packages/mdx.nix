{
  lib,
  buildGoModule,
  fetchFromGitHub,
  ...
}:
let
  pname = "mdx";
  version = "1.11.1";
in
buildGoModule {
  inherit pname version;

  src = fetchFromGitHub {
    owner = "arimatakao";
    repo = "mdx";
    rev = "v${version}";
    hash = "sha256-bXf6TIVCd7EknyFKeE12YOcqnZ4F3tDFNWBydO/DQAA=";
  };

  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
  ];

  # test suite requires network
  doCheck = false;

  vendorHash = "sha256-+q8OQNU43E0Hdpinj+FSZdf+Nirdemc/Is4mZC8YAcs=";

  meta = {
    description = "Command-line interface program for downloading manga from the MangaDex website";
    changelog = "https://github.com/arimatakao/mdx/releases/tag/v${version}";
    homepage = "https://github.com/arimatakao/mdx";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ luftmensch-luftmensch ];
    mainProgram = "mdx";
  };
}
