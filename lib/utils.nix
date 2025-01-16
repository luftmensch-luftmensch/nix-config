{
  lib,
  pkgs,
  ...
}: {
  builders = {
    # Example usage:
    # 1. Define our extensions
    # extensions = {
    #   "bitwarden" = buildFirefoxXpiAddon {
    #     pname = "bitwarden";
    #     version = "2024.6.2";
    #     addonId = "{446900e4-71c2-419f-a6a7-df9c091e268b}";

    #     src = pkgs.fetchurl {
    #       url = "https://addons.mozilla.org/firefox/downloads/file/4305759/bitwarden_password_manager-2024.6.2.xpi";
    #       hash = "sha256-wGTi1mAcuSHs0VTg07/VTXGvQ9oZR6pRZmh37wr9FDY=";
    #     };

    #     meta = {
    #       license = lib.licenses.gpl3;
    #       platforms = lib.platforms.all;
    #     };
    #   };
    # };
    # Use it inside firefox
    # programs.firefox.profiles.arkenfox = {
    #   extensions = builtins.attrValues extensions;
    # };
    buildFirefoxXpiAddon = lib.makeOverridable ({
      src,
      pname,
      version,
      addonId,
      meta,
      ...
    }:
      pkgs.stdenv.mkDerivation {
        name = "${pname}-${version}";

        inherit meta src;

        preferLocalBuild = true;
        allowSubstitutes = true;

        passthru = {inherit addonId;};

        buildCommand = ''
          dst="$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}"
          mkdir -p "$dst"
          install -v -m644 "$src" "$dst/${addonId}.xpi"
        '';
      });
  };

  wrappers = {
    # ref: https://discourse.nixos.org/t/how-to-write-a-electron-app-wrap-function/40581
    # example: discord = electron { appName = "discord"; }
    electron = {appName}:
      pkgs.symlinkJoin {
        name = appName;
        paths = [pkgs.${appName}];
        buildInputs = [pkgs.makeWrapper];
        postBuild = lib.strings.concatStrings [
          "wrapProgram $out/bin/"
          appName
          " --add-flags \"--enable-webrtc-pipewire-capturer\""
          " --add-flags \"--enable-features=WaylandWindowDecorations\""
          " --add-flags \"--enable-wayland-ime\""
        ];
      };

    # Progressive Web App around chrome
    pwa = {
      name,
      url,
      icon,
      package ? pkgs.chromium,
    }:
      pkgs.makeDesktopItem {
        inherit name icon;
        desktopName = name;
        genericName = name;
        exec = ''
          ${lib.getExe package} --ozone-platform-hint=auto --force-dark-mode --enable-features=WebUIDarkMode --app="${url}"
        '';

        categories = [
          "Network"
          "InstantMessaging"
        ];

        mimeTypes = ["x-scheme-handler/${name}"];
      };
  };
}
