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
      # version = "1.8.22";
      version = "1.8.23";
      src = final.fetchFromGitHub {
        owner = "tdlib";
        repo = "td";

        # The tdlib authors do not set tags for minor versions, but
        # external programs depending on tdlib constrain the minor
        # version, hence we set a specific commit with a known version.
        rev = "d963044eb9b8bb075e3f63b8bfd8da735c4c37d9";
        sha256 = "0wf7y805hrzjzmvjkmkb1hxr133466n4qi6amrlhqgjndnb4rm8j";

        # rev = "85c8c19b7fddf4188a730486f05dcbf6bb855aab";
        # sha256 = "13gabrcqm5djgcr01iiv6kzba0pmixw9wg5r8gkxwl1qdjp0qxr7";
      };
    });
  };
}
