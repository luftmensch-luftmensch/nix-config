{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.packages.base;
in {
  options.system.modules.packages.base = {
    enable = mkEnableOption "Enable base packages used across all my devices";
  };

  config = mkIf cfg.enable {

    environment.systemPackages = with pkgs; [
      tdlib                    # Cross-platform library for building Telegram clients
    ];
  };
}
