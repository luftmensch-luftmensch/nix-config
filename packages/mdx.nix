{
  lib,
  buildGoModule,
  fetchFromGitHub,
  ...
}:
let
  pname = "mdx";
  version = "1.13.0";
in
buildGoModule {
  inherit pname version;

  src = fetchFromGitHub {
    owner = "arimatakao";
    repo = "mdx";
    rev = "v${version}";
    hash = "sha256-06XdBt/4WXyKzKKE1UF5lw86MWS2+2IfN2jJh2OVdDo=";
  };

  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
  ];

  # test suite requires network
  doCheck = false;

  vendorHash = "sha256-r3XvQr4Uzxt04hnmI4CeBumu1Fg7dMsW6THTI4ah3Eo=";

  meta = {
    description = "Command-line interface program for downloading manga from the MangaDex website";
    changelog = "https://github.com/arimatakao/mdx/releases/tag/v${version}";
    homepage = "https://github.com/arimatakao/mdx";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ luftmensch-luftmensch ];
    mainProgram = "mdx";
  };
}
