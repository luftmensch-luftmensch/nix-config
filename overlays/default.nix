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
      # version = "1.8.20";
      version = "1.8.21";
      src = final.fetchFromGitHub {
        owner = "tdlib";
        repo = "td";

        # The tdlib authors do not set tags for minor versions, but
        # external programs depending on tdlib constrain the minor
        # version, hence we set a specific commit with a known version.
        # rev = "2589c3fd46925f5d57e4ec79233cd1bd0f5d0c09";
        # hash = "sha256-mbhxuJjrV3nC8Ja7N0WWF9ByHovJLmoLLuuzoU4khjU=";
        # rev = "dd77e4628f1a65f332f0cb81c82a19c0fcfa40c2";
        # sha256 = "1j1ckya3dxh2jkxa0xbnpxmbq407cc6z15qf6pn2c496jap2k7h0";
        rev = "6ee64289f3666774fb694c9d33f83ed8bd52b60c";
        sha256 = "0n1wnsp34ibyw4g3a7307h9b8jqnzgsfhma1b0zkxqb4625z9yqx";
      };
    });
  };
}
