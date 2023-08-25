inputs: {
  # Overlays for personal pkgs (callPackage)
  additions = final: _: import ../packages {pkgs = final;};

  # Overlays for various pkgs (e.g. broken / too old)
  modifications = final: prev: {
    stable = import inputs.nixpkgs-stable {
      system = final.system;
      config.allowUnfree = true; # Forgive me Stallman
    };

    # waybar = prev.waybar.overrideAttrs (oldAttrs: {
    #   mesonFlags = oldAttrs.mesonFlags ++ ["-Dexperimental=true"];
    # });

    tdlib = prev.tdlib.overrideAttrs (oldAttrs: {
      version = "1.8.16";
      src = final.fetchFromGitHub {
        owner = "tdlib";
        repo = "td";
        # rev = "66234ae2537a99ec0eaf7b0857245a6e5c2d2bc9";
        # sha256 = "0lv19vpgv8nqzxmsgnmbg32zjkarqngh8cjhxc3hy3jj74cwkil5";
        rev = "470c36ceef780ff6189bbd8e2f889941ca828247";
        sha256 = "1mb36iix7r4x429zpwx5wpjh97c00lvzbx21pd5n56d1xjdvqqv6";

      };
    });
  };
}
