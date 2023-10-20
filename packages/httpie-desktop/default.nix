{
  pkgs,
  # lib,
  ...
}: let
  pname = "httpie-desktop";
  version = "2023.3.6";
  name = "${pname}-${version}";

  src = pkgs.fetchurl {
    url = "https://github.com/httpie/desktop/releases/download/v${version}/HTTPie-${version}.AppImage";
		sha256 = "1qq8rfk5jm7k2ncp5q01zzbkhai3p9dxrmllc8nw8z3knmjzfw00";
  };

	# appimageContents = pkgs.appimageTools.extractType2 {inherit name src;};

	# install -m 444 -D ${appimageContents}/${pname}.desktop $out/share/applications/${pname}.desktop

	# install -m 444 -D ${appimageContents}/${pname}.png $out/share/icons/hicolor/512x512/apps/${pname}.png
	# substituteInPlace $out/share/applications/${pname}.desktop \ --replace 'Exec=AppRun --no-sandbox %U' 'Exec=${pname} %U'
in
  pkgs.appimageTools.wrapType2 rec {
    inherit name src;

    extraInstallCommands = ''
      mv $out/bin/${name} $out/bin/${pname}
    '';

    meta = {
      description = "Cross-platform API testing client for humans. Painlessly test REST, GraphQL, and HTTP APIs.";
      homepage = "https://github.com/httpie/desktop";
      # license = licenses.gpl3;
      maintainers = [];
      # platforms = ["x86_64-linux"];
    };
  }
