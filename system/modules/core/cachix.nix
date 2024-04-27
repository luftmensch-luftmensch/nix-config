{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.core.cachix;
in {
  options.system.modules.core.cachix.enable = mkEnableOption "cachix configuration";

  config = mkIf cfg.enable {
    environment.systemPackages = [pkgs.cachix];

    nix.settings = {
      substituters = [
        "https://cache.nixos.org/" #  Prebuilt binaries for Nixpkgs and NixOS. Used automatically by the Nix pm to speed up builds.
        "https://nix-community.cachix.org" # Nix community binary cache
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" # cache.nixos.org
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" # nix-community
      ];
    };
  };
}
