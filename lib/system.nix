{inputs, ...}:
with builtins; let
  inherit (inputs) self nixpkgs;
  inherit (nixpkgs) lib;
  inherit (self) outputs overlays nixosModules;

  genConfiguration = {
    hostname,
    system,
    stateVersion,
    ...
  }: let
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = attrValues overlays;
    };

    baseSystem = {
      nixpkgs = {inherit pkgs;};
      system = {inherit stateVersion;};
      networking.hostName = lib.mkDefault hostname;
    };
  in
    lib.nixosSystem {
      inherit system;

      modules =
        [
          baseSystem
          inputs.impermanence.nixosModules.impermanence
          "${self}/system/hosts/${hostname}"
        ]
        ++ attrValues nixosModules;

      specialArgs = {
        inherit inputs outputs;
      };
    };
in
  genConfiguration
