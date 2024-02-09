{
  pkgs,
  # lib,
  ...
}: let
  pname = "httpie-desktop";
  version = "2024.1.2";
  name = "${pname}-${version}";

  src = pkgs.fetchurl {
    url = "https://github.com/httpie/desktop/releases/download/v${version}/HTTPie-${version}.AppImage";
    sha256 = "sha256-OOP1l7J2BgO3nOPSipxfwfN/lOUsl80UzYMBosyBHrM=";
  };
in
  pkgs.appimageTools.wrapType2 rec {
    inherit name src;

    extraInstallCommands = ''
      mv $out/bin/${name} $out/bin/${pname}
    '';

    meta = {
      description = "Cross-platform API testing client for humans. Painlessly test REST, GraphQL, and HTTP APIs.";
      homepage = "https://github.com/httpie/desktop";
      maintainers = [];
    };
  }
