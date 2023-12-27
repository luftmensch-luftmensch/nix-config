{inputs, ...}:
with builtins; let
  inherit (inputs) self nixpkgs home-manager nur;
  inherit (self) outputs overlays homeModules;

  genConfiguration = {
    username,
    hostname,
    system,
    stateVersion,
    ...
  }: let
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = attrValues overlays ++ [
        nur.overlay
      ];
    };

    baseHome = {
      inherit username;
      inherit stateVersion;
      homeDirectory = "/home/${username}";
    };
  in
    home-manager.lib.homeManagerConfiguration {
      inherit pkgs;

      modules = let
        configs = "${self}/home/${username}";
      in
        [
          {home = baseHome;}
          "${configs}/hosts/${hostname}.nix"
        ]
        ++ [inputs.nix-index-database.hmModules.nix-index]
        ++ attrValues homeModules.${username};

      extraSpecialArgs = {inherit inputs outputs;};
    };
in
  genConfiguration
