# > Where the snow paradise begins
#
# Author:  luftmensch-luftmensch
# URL:     https://github.com/luftmensch-luftmensch/Nixos/
# License: GPL-3.0
# You probably shouldn't be using my configuration as a guideline
# Sure, it works (mostly), but the code is horrible
# I have no idea what the hell I'm doing... It's true!
#
# You can get started with flakes here: https://nixos.wiki/wiki/Flakes
# Also, you may want to take a look on the flakes that I took inspiration:
# - https://github.com/ners/NixOS
# - https://github.com/balsoft/nixos-config
# - https://github.com/Kranzes/nix-config
# - https://github.com/jonringer/nixpkgs-config
# - https://github.com/sebastiant/dotfiles
# - https://github.com/kotokrad/dotfiles (fennel nvim!)
#
# Welcome to ground zero. Where the whole flake gets set up and all its modules
# are loaded.
{
  description = "A collection of crap, hacks and copy-paste to make my localhosts boot";

  # Attribute set of all the dependencies used in the flake - Dependency Management Part
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11"; # Released in 30.11.2023
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    ### Handle persistent state on systems with ephemeral root storage ###
    impermanence.url = "github:nix-community/impermanence";

    ###      Atomic secret provisioning for NixOS based on sops      ###
    # sops-nix = {
    #   url = "github:Mic92/sops-nix";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
    neovim-flake.url = "github:luftmensch-luftmensch/neovim-flake";
  };

  # Function of an argument that uses a the inputs for reference
  # - Configure what you imported
  # - Can be pretty much anything: Packages / configurations / modules / etc...
  outputs = inputs @ {
    nixpkgs,
    nixpkgs-unstable,
    impermanence,
    # sops-nix,
    neovim-flake,
    ...
  }: let
    lib = import ./lib {inherit inputs;};
    inherit (lib) mkSystem forAllSystems;
  in {
    nixosModules = import ./system/modules;
    overlays = import ./overlays inputs;
    formatter = forAllSystems (
      system:
        nixpkgs.legacyPackages.${system}.alejandra
    );

    packages = forAllSystems (
      system:
        import ./packages {pkgs = nixpkgs.legacyPackages.${system};}
    );

    devShells = forAllSystems (
      system:
        import ./shell.nix {pkgs = nixpkgs.legacyPackages.${system};}
    );

    nixosConfigurations = {
      # Thinkpad T14-S GEN2 - Laptop
      kronos = mkSystem {
        hostname = "kronos";
        system = "x86_64-linux";
        stateVersion = "22.05";
      };

      # Lenovo IdeaCentre K450 - Desktop PC
      atlas = mkSystem {
        hostname = "atlas";
        system = "x86_64-linux";
        stateVersion = "22.05";
      };
    };
  };
}
