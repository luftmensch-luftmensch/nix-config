{
  lib,
  buildGoModule,
  fetchFromGitHub,
  ...
}:
let
  version = "0.23.0";
in
buildGoModule {
  pname = "meteor";
  inherit version;

  src = fetchFromGitHub {
    owner = "stefanlogue";
    repo = "meteor";
    rev = "v${version}";
    hash = "sha256-UA6bye9ti9AwnZkBIGjljDElkIEOhsiJ0NyYdKbF5m0=";
  };

  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
  ];

  vendorHash = "sha256-jKd/eJwp5SZvTrP3RN7xT7ibAB0PQondGR3RT+HQXIo=";

  meta = {
    description = "Highly configurable CLI tool for writing conventional commits";
    changelog = "https://github.com/stefanlogue/meteor/releases/tag/v${version}";
    homepage = "https://github.com/stefanlogue/meteor";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ luftmensch-luftmensch ];
    mainProgram = "meteor";
  };
}
