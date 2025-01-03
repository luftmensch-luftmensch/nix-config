{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.system.modules.core.slim;
in
{
  # Taken from: https://github.com/NixOS/nixpkgs/pull/330440 -> https://github.com/NuschtOS/nixos-modules/blob/main/modules/slim.nix
  options.system.modules.core.slim.enable = mkEnableOption "disable some normally rarely used things to slim down the system";

  # config = mkIf cfg.enable { };
  config = lib.mkIf cfg.enable (
    {
      nixpkgs.overlays = lib.mkIf (!config.programs.thunderbird.enable or true) [
        (_final: prev: {
          thunderbird = prev.thunderbird.override { cfg.speechSynthesisSupport = false; };
        })
      ];

    }
    // lib.optionalAttrs (lib.versionAtLeast lib.version "24.11") {
      programs.thunderbird.package = pkgs.thunderbird.override { cfg.speechSynthesisSupport = false; };
    }
    // {

      services =
        lib.optionalAttrs (config.services ? orca) {
          orca.enable = false; # requires speechd
        }
        // lib.optionalAttrs (config.services ? speechd) {
          speechd.enable = false; # voice files are big and fat
        };
    }
  );
}
