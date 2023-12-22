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

    tdlib = prev.tdlib.overrideAttrs (oldAttrs: {
      # version = "1.8.21";
      version = "1.8.22";
      src = final.fetchFromGitHub {
        owner = "tdlib";
        repo = "td";

        # The tdlib authors do not set tags for minor versions, but
        # external programs depending on tdlib constrain the minor
        # version, hence we set a specific commit with a known version.
        # rev = "6ee64289f3666774fb694c9d33f83ed8bd52b60c";
        # sha256 = "0n1wnsp34ibyw4g3a7307h9b8jqnzgsfhma1b0zkxqb4625z9yqx";
        rev = "85c8c19b7fddf4188a730486f05dcbf6bb855aab";
        sha256 = "13gabrcqm5djgcr01iiv6kzba0pmixw9wg5r8gkxwl1qdjp0qxr7";
      };
    });
  };
}
