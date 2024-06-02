{ config, lib, ... }:
with lib;
let
  cfg = config.system.modules.graphical.xorg;
in
{
  options.system.modules.graphical.xorg.enable = mkEnableOption "xorg basic configuration and packages";

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      xkb.layout = "it";
      windowManager.i3.enable = true;
    };

    services.displayManager.defaultSession = "none+i3";
  };
}
