{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  nix = {
    package = pkgs.nixFlakes;

    settings = {
      trusted-users = ["root" "@wheel"];
      allowed-users = ["root" "@wheel"];

      auto-optimise-store = lib.mkDefault true;
      warn-dirty = false;
    };

    gc = {
      automatic = true;
      dates = "Monday 09:00";
      options = "--delete-older-than 7d";
    };

    extraOptions = ''
      keep-outputs          = true
      keep-derivations      = true
      experimental-features = nix-command flakes repl-flake
    '';

    # Add each flake input as a registry
    # To make nix3 commands consistent with the flake
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;

    # Map registries to channels
    # Very useful when using legacy commands
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;
  };
}
