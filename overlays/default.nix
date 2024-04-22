inputs: {
  # Overlays for personal pkgs (callPackage)
  additions = final: _: import ../packages {pkgs = final;};

  # Overlays for various pkgs (e.g. broken / too old)
  modifications = final: prev: {
    stable = import inputs.nixpkgs-stable {
      inherit (final) system;
      config.allowUnfree = true; # Forgive me Stallman
    };

    # waybar = prev.waybar.overrideAttrs (oldAttrs: {
    #   mesonFlags = oldAttrs.mesonFlags ++ ["-Dexperimental=true"];
    # });

    rofi-emoji-wayland = prev.rofi-emoji.overrideAttrs (_oldAttrs: {
      buildInputs = with final; [
        rofi-wayland-unwrapped
        cairo
        glib
        libnotify
        wl-clipboard
        xclip
        xsel
      ];
    });

    tdlib = prev.tdlib.overrideAttrs (_oldAttrs: {
      version = "1.8.27";
      src = final.fetchFromGitHub {
        owner = "tdlib";
        repo = "td";

        # The tdlib authors do not set tags for minor versions, but
        # external programs depending on tdlib constrain the minor
        # version, hence we set a specific commit with a known version.
        rev = "d7203eb719304866a7eb7033ef03d421459335b8";
        sha256 = "160d1ijiwh3px0v8ggsqxh39dnb248pjnvgw0ncyw459ywn3y5ba";
      };
    });
  };
}
