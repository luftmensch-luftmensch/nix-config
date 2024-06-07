{
  pkgs,
  lib,
  inputs,
  config,
  ...
}:
{
  nix = {
    package = pkgs.nixVersions.latest;

    settings = {
      trusted-users = [
        "root"
        "@wheel"
        "nix-builder"
      ];
      allowed-users = [
        "root"
        "@wheel"
        "nix-builder"
      ];

      auto-optimise-store = lib.mkDefault true;

      # let the system decide the number of max jobs
      max-jobs = "auto";
      warn-dirty = false;

      # show more log lines for failed builds
      log-lines = 30;

      # enable new nix command and flakes
      # and also "unintended" recursion as well as content addressed nix
      extra-experimental-features = [
        "flakes" # flakes
        "nix-command" # experimental nix commands
        "recursive-nix" # let nix invoke itself
        "ca-derivations" # content addressed nix
        "auto-allocate-uids" # allow nix to automatically pick UIDs, rather than creating nixbld* user accounts
        "configurable-impure-env" # allow impure environments
        "cgroups" # allow nix to execute builds inside cgroups
        "git-hashing" # allow store objects which are hashed via Git's hashing algorithm
        "verified-fetches" # enable verification of git commit signatures for fetchGit
      ];

      # for direnv GC roots
      keep-outputs = true;
      keep-derivations = true;
    };

    gc = {
      automatic = true;
      dates = "Tue *-*-* 18:00:00";
      options = "--delete-older-than 14d";
    };

    # Add each flake input as a registry
    # To make nix3 commands consistent with the flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # Map registries to channels
    # Very useful when using legacy commands
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;
  };
}
