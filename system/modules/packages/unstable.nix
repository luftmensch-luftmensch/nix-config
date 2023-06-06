{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:

with lib; let
  cfg = config.system.modules.packages.unstable;
in {
  options.system.modules.packages.unstable = {
    enable = mkEnableOption "Enable packages pulled from unstable branch";
  };

  config = mkIf cfg.enable {

    environment.systemPackages = with inputs.nixpkgs-unstable.legacyPackages.x86_64-linux; [
      swaynotificationcenter    # Simple notification daemon with a GUI built for Sway
    ];
  };
}
