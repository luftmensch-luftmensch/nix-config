{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.system.modules.packages.unstable;
  unstable-pkgs = inputs.nixpkgs-unstable.legacyPackages.x86_64-linux;
in {
  options.system.modules.packages.unstable = {
    enable = mkEnableOption "Enable packages pulled from unstable branch";
    onWayland = mkEnableOption "[Wayland only] Enable packages pulled from unstable branch";
  };

  # config = mkIf cfg.enable {
  #   environment.systemPackages = with inputs.nixpkgs-unstable.legacyPackages.x86_64-linux; [
  #     swaynotificationcenter    # Simple notification daemon with a GUI built for Sway
  #   ];
  # };

  config = mkIf cfg.enable (
    mkMerge [
      {
        environment.systemPackages = with unstable-pkgs; [
          # Moved here until https://github.com/NixOS/nixpkgs/pull/262797 get fixed
          bitwarden
        ];
      }
      (mkIf cfg.onWayland {
        environment.systemPackages = with unstable-pkgs; [
          swaynotificationcenter # Simple notification daemon with a GUI built for Sway
        ];
      })
    ]
  );
}
