{ inputs, ... }:
with builtins;
let
  inherit (inputs)
    self
    nixpkgs
    nixpkgs-unstable
    home-manager
    ;
  inherit (self) outputs overlays homeModules;

  genConfiguration =
    {
      username,
      hostname,
      system,
      stateVersion,
      ...
    }:
    let
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = attrValues overlays;
      };

      unstable-pkgs = import nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
        overlays = attrValues overlays;
      };

      baseHome = {
        inherit username;
        inherit stateVersion;
        homeDirectory = "/home/${username}";
      };

      infra = import ./infra.nix {
        inherit (nixpkgs) lib;
        inherit pkgs;
      };
    in
    home-manager.lib.homeManagerConfiguration {
      inherit pkgs;

      modules =
        let
          configs = "${self}/home/${username}";
        in
        [
          { home = baseHome; }
          "${configs}/hosts/${hostname}.nix"
        ]
        ++ attrValues homeModules.${username}
        ++ [ inputs.nur.modules.homeManager.default ];

      extraSpecialArgs = {
        inherit
          inputs
          outputs
          unstable-pkgs
          infra
          ;
      };
    };
in
genConfiguration
