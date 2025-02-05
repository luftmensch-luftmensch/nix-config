inputs: {
  # Overlays for personal pkgs (callPackage)
  additions = final: _: import ../packages { pkgs = final; };

  # Overlays for various pkgs (e.g. broken / too old)
  modifications = final: prev: {
    stable = import inputs.nixpkgs {
      inherit (final) system;
      config.allowUnfree = true; # Forgive me Stallman
    };

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

    # tdlib = prev.tdlib.overrideAttrs (_oldAttrs: {
    #   version = "1.8.39";
    #   src = final.fetchFromGitHub {
    #     owner = "tdlib";
    #     repo = "td";
    #
    #     # The tdlib authors do not set tags for minor versions, but
    #     # external programs depending on tdlib constrain the minor
    #     # version, hence we set a specific commit with a known version.
    #     rev = "2be9e799a2bc523550d4f83f4d2d66d41c9573b9";
    #     hash = "sha256-avtCYezgwA3CnSYGJg1Z+GVRLOW6Gpq415/YJvBPVhQ=";
    #   };
    # });

    mpv-visualizer = prev.mpvScripts.visualizer.overrideAttrs (_oldAttrs: {
      patches = [ ./patches/visualizer.patch ];
    });
  };
}
