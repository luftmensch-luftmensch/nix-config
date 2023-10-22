{
  pkgs,
  lib,
  ...
}: let
  pname = "insomnium";
  version = "0.2.1-b";
  name = "${pname}-${version}";

  src = pkgs.fetchurl {
    url = "https://github.com/ArchGPT/insomnium/releases/download/core%400.2.1-b/Insomnium.Core-${version}.AppImage";
    
		sha256 = "0q4bdlj4sj6ggpmkyd6yvahjc0lwwmd247jrw545rx0x20fxil4f";
  };

in
  pkgs.appimageTools.wrapType1 rec { # ISO 9660 file that are also ELF executables.
    inherit name src;

    extraInstallCommands = ''
      mv $out/bin/${name} $out/bin/${pname}
    '';

    meta = with lib; {
      description = "Fast local API testing tool that is privacy-focused and 100% local";
      homepage = "https://github.com/ArchGPT/insomnium";
      license = licenses.mit;
      maintainers = [];
      # platforms = ["x86_64-linux"];
    };
  }
