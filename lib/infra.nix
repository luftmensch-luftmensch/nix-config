{
  lib,
  pkgs,
  ...
}:
{
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
    buildFirefoxXpiAddon = lib.makeOverridable (
      {
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

        passthru = { inherit addonId; };

        buildCommand = ''
          dst="$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}"
          mkdir -p "$dst"
          install -v -m644 "$src" "$dst/${addonId}.xpi"
        '';
      }
    );
  };

  wrappers = {
    # Progressive Web App around chrome
    pwa =
      {
        name,
        url,
        package ? pkgs.chromium,
      }:
      pkgs.writeShellScriptBin name ''
        exec ${lib.getExe package} \
          --add-flags "--enable-webrtc-pipewire-capturer" \
          --add-flags "--enable-features=WaylandWindowDecorations" \
          --add-flags "--enable-wayland-ime" \
          --add-flags "--ozone-platform-hint=auto" \
          --add-flags "--force-dark-mode" \
          --add-flags "--enable-features=WebUIDarkMode" \
          --add-flags "--app=${url}"
      '';
  };
}
