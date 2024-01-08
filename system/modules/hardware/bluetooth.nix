{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.hardware.bluetooth;
in {
  options.system.modules.hardware.bluetooth = {
    enable = mkEnableOption "Enable bluetooth capabilities";
  };

  config = mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      settings = {
        General = {
          Experimental = true;
        };
      };
    };

    environment.systemPackages = [pkgs.bluez];
  };
}
