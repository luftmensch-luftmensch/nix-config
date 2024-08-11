{
  lib,
  buildGoModule,
  fetchFromGitHub,
  ...
}:
let
  version = "1.11.0";
in
buildGoModule {
  pname = "mdx";
  inherit version;

  src = fetchFromGitHub {
    owner = "arimatakao";
    repo = "mdx";
    rev = "v${version}";
    hash = "sha256-orlXXJAhvhnvRbnzMba7725pmkcDTIQnqCMwStn+5Qw=";
  };

  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
  ];

  # test suite requires network
  doCheck = false;

  vendorHash = "sha256-xHN+OrqodeK2vVZjGsH88OuHVcNtLbM+B4K5Ty45q0o=";

  meta = {
    description = "Command-line interface program for downloading manga from the MangaDex website";
    changelog = "https://github.com/arimatakao/mdx/releases/tag/v${version}";
    homepage = "https://github.com/arimatakao/mdx";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ luftmensch-luftmensch ];
    mainProgram = "mdx";
  };
}
