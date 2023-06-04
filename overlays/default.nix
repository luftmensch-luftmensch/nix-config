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
      version = "1.8.12";
      src = final.fetchFromGitHub {
        owner  = "tdlib";
        repo   = "td";
        rev    = "c95598e5e1493881d31211c1329bdbe4630f6136";
        sha256 = "0h435zh4wylzvxd6chzjawa9zibmnnza8nf01zqbaywfg17vpcbp";
      };

    });
  };
}
