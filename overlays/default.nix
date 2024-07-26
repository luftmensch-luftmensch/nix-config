inputs: {
  # Overlays for personal pkgs (callPackage)
  additions = final: _: import ../packages { pkgs = final; };

  # Overlays for various pkgs (e.g. broken / too old)
  modifications = final: prev: {
    stable = import inputs.nixpkgs {
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
      version = "1.8.29";
      src = final.fetchFromGitHub {
        owner = "tdlib";
        repo = "td";

        # The tdlib authors do not set tags for minor versions, but
        # external programs depending on tdlib constrain the minor
        # version, hence we set a specific commit with a known version.
        rev = "97ded01095246a3a693bc85bef4bca5d1af177dd";
        sha256 = "sha256-KCcXltbCiN9b0OkGkXqLRq8sTgIToy8RhPP4ILR1gk0=";
      };
    });

    mpv-visualizer = prev.mpvScripts.visualizer.overrideAttrs (_oldAttrs: {
      patches = [ ./patches/visualizer.patch ];
    });
  };
}
