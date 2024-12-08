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

    tdlib = prev.tdlib.overrideAttrs (_oldAttrs: {
      version = "1.8.39";
      src = final.fetchFromGitHub {
        owner = "tdlib";
        repo = "td";

        # The tdlib authors do not set tags for minor versions, but
        # external programs depending on tdlib constrain the minor
        # version, hence we set a specific commit with a known version.
        rev = "056963e48fa8d3f89556239c22d6ac843d3c8a5b";
        hash = "sha256-pQ+uYnyBTH4L6RW3MqAttx7K3z8fyEbzRy8JSjbBL98=";
      };
    });

    mpv-visualizer = prev.mpvScripts.visualizer.overrideAttrs (_oldAttrs: {
      patches = [ ./patches/visualizer.patch ];
    });
  };
}
